#!/usr/bin/python

import sys
import string
import re

NODECL_PREFIX="NODECL_"

def loadlines(f):
    lines = f.readlines()
    result = []
    for l in lines:
        l = l.strip(" \n")
        if l == "" or l[0] == "#":
            continue
        result.append(l)
    return result

def find_matching_parentheses(s):
    level = 0;
    for i in range(0, len(s)):
        if s[i] == '(':
           level = level + 1
        elif s[i] == ')':
           level = level - 1
           if level == 0:
              return i;
    return -1

def parse_rules(f):
    lines = loadlines(f)
    
    rule_set = []
    
    rule_name = ""
    rule_rhs = []
    for l in lines:
       colon = l.find(":")
       if (colon >= 0):
           if (rule_name != "") :
               rule_set.append( (rule_name, rule_rhs) )
           rule_name = l[:colon].strip()
           rule_rhs = [ l[colon + 1 :].strip() ]
       elif l[0] == '|':
           rule_rhs.append(l[1:].strip())
    # Last rule
    if (rule_name != "") :
       rule_set.append( (rule_name, rule_rhs) )
    
    regex_name = re.compile("\[([_A-Za-z][_A-Za-z0-9]*)\]\s*([-_A-Za-z0-9]+)")
    rule_map = { }
    for r in rule_set:
        (rule_name, rule_rhs) = r
        if rule_name not in rule_map:
            rule_map[rule_name] = [] 
        for rhs in rule_rhs:
            # Assume this is a NODECL_ name
            if (rhs[0:len(NODECL_PREFIX)] == NODECL_PREFIX):
                i = rhs.find("(")
                if i < 0:
                    raise Exception("invalid syntax, expecting (")
                tree_ast = rhs[0:i].strip()
                j = find_matching_parentheses(rhs[i:])
                ast_args = rhs[i + 1 : i + j]
                ast_args = ast_args.strip()
                remaining_flags = rhs[i+j+1:].split();
                needs_symbol = "symbol" in remaining_flags
                needs_type = "type" in remaining_flags
                needs_text = "text" in remaining_flags
                needs_cval = "const_value" in remaining_flags
                needs_template_parameters = "template-parameters" in remaining_flags
                needs_decl_context = "context" in remaining_flags
                if ast_args :
                    ast_args_2 = map(lambda x : x.strip(), ast_args.split(","))
                    ast_args_3 = []
                    i = 0
                    for x in ast_args_2:
                        m = regex_name.match(x)
                        if m:
                            (r_label, r_ref) = m.groups()
                        else:
                            (r_label, r_ref) = (x, x)
                            sys.stderr.write("Missing label for component %d in %s\n" % (i, rhs))
                        ast_args_3.append((r_label, r_ref))
                        i = i + 1
                        
                    rule_map[rule_name].append( NodeclStructure(tree_ast, ast_args_3, needs_symbol, needs_type, \
                                needs_text, needs_cval, needs_template_parameters, needs_decl_context) )
                else:
                    rule_map[rule_name].append( NodeclStructure(tree_ast, [], needs_symbol, needs_type, \
                                needs_text, needs_cval, needs_template_parameters, needs_decl_context) )
            else:
                rule_map[rule_name].append( RuleRef(rhs) )
    return rule_map


class Variable:
    pass

class NodeclStructure(Variable):
    def __init__(self, tree_kind, subtrees, needs_symbol, needs_type, needs_text, needs_cval, needs_template_parameters, needs_decl_context):
        self.tree_kind = tree_kind
        self.subtrees = subtrees
        self.needs_symbol = needs_symbol
        self.needs_type = needs_type
        self.needs_text = needs_text
        self.needs_cval = needs_cval
        self.needs_template_parameters = needs_template_parameters
        self.needs_decl_context = needs_decl_context
    def is_nullable(self, already_seen = []):
        return False
    def first(self, already_seen = []) :
        return set([self.tree_kind])
    def check_function_name(self):
        return "nodecl_check_%s" % (self.tree_kind)
    def call_to_check(self, tree_name):
        return "%s(%s);" % (self.check_function_name(), tree_name);
    def function_check_code(self):
        print "static void %s(nodecl_t n)" % (self.check_function_name())
        print "{"
        print "ERROR_CONDITION(nodecl_is_null(n), \"Node is null\", 0);"
        print "ERROR_CONDITION(nodecl_get_kind(n) != %s, \"Invalid node\", 0);" % self.tree_kind
        if (self.needs_symbol):
           print "   ERROR_CONDITION(nodecl_get_symbol(n) == NULL, \"Tree lacks a symbol\", 0);" 
        if (self.needs_type):
           print "   ERROR_CONDITION(nodecl_get_type(n) == NULL, \"Tree lacks a type\", 0);" 
        if (self.needs_text):
           print "   ERROR_CONDITION(nodecl_get_text(n) == NULL, \"Tree lacks an associated text\", 0);" 
        if (self.needs_cval):
           print "   ERROR_CONDITION(nodecl_get_constant(n) == NULL, \"Tree lacks a constant value\", 0);" 
        i = 0
        for subtree in self.subtrees:
           (rule_label, rule_ref) = subtree

           current_rule = RuleRef(rule_ref)

           print current_rule.call_to_check("nodecl_get_child(n, %d)" % (i));

           i = i + 1
        print "}"

class RuleRef(Variable):
    def __init__(self, rule_ref):
        self.rule_ref = rule_ref
    def normalize_rule_name(self, rule_ref):
        is_seq = rule_ref.find("-seq") > 0
        is_opt = rule_ref.find("-opt") > 0
        rule_ref = rule_ref.replace("-seq", "").replace("-opt", "")
        rule_ref_c = rule_ref.replace("-", "_")
        return (rule_ref, rule_ref_c, is_seq, is_opt) 
    def canonical_rule(self):
        (rule_ref, rule_ref_c, is_seq, is_opt) = self.normalize_rule_name(self.rule_ref)
        return rule_ref
    def is_canonical_rule(self):
        (rule_ref, rule_ref_c, is_seq, is_opt) = self.normalize_rule_name(self.rule_ref)
        return rule_ref == self.rule_ref
    def is_opt(self):
        (rule_ref, rule_ref_c, is_seq, is_opt) = self.normalize_rule_name(self.rule_ref)
        return is_opt
    def is_seq(self):
        (rule_ref, rule_ref_c, is_seq, is_opt) = self.normalize_rule_name(self.rule_ref)
        return is_seq
    def is_nullable(self, _already_seen = []):
        (rule_ref, rule_ref_c, is_seq, is_opt) = self.normalize_rule_name(self.rule_ref)
        already_seen = _already_seen[:]
        if is_opt:
            return True
        if rule_ref in already_seen:
           return False
        already_seen.append(rule_ref)
        rule_set = rule_map[rule_ref]
        for rhs in rule_set:
          if rhs.is_nullable(already_seen):
            return True
        return False
    def first(self, _already_seen = []) :
        already_seen = _already_seen[:]
        (rule_ref, rule_ref_c, is_seq, is_opt) = self.normalize_rule_name(self.rule_ref)
        if rule_ref in already_seen:
           return set([])
        if is_seq:
            return set(["AST_NODE_LIST"])
        already_seen.append(rule_ref)
        rule_set = rule_map[rule_ref]
        s = set([])
        for rhs in rule_set:
            s = s.union(rhs.first(already_seen))
        return s
    def check_function_name(self):
        return "nodecl_check_%s" % (self.canonical_rule().replace("-", "_"))
    def call_to_check(self, tree_name):
        if self.is_nullable():
            if self.is_seq():
               return "nodecl_check_nullable_list_rule(%s, %s);" % (tree_name, self.check_function_name()) 
            else:
               return "nodecl_check_nullable_rule(%s, %s);" % (tree_name, self.check_function_name())
        else:
            if self.is_seq():
               return "nodecl_check_list_rule(%s, %s);" % (tree_name, self.check_function_name()) 
            else:
               return "%s(%s);" % (self.check_function_name(), tree_name)
    def function_check_code(self):
        if not self.is_canonical_rule():
            raise Exception("Do not call this on non canonical rules")

        rule_set = rule_map[self.rule_ref]
        print "static void %s(nodecl_t n)" % (self.check_function_name())
        print "{"
        print "ERROR_CONDITION(nodecl_is_null(n), \"Node is null\", 0);"
        print "switch (nodecl_get_kind(n))"
        print "{"
        for rhs in rule_set:
            first_set = rhs.first()
            if not first_set:
               raise Exception("First is empty!")
            for first in first_set:
                print "case %s:" % (first)
            print "{"
            print rhs.call_to_check("n");
            print "break;"
            print "}"

        print "default:"
        print "{"
        print "internal_error(\"Node of kind %s not valid\", ast_print_node_type(nodecl_get_kind(n)));"
        print "break;"
        print "}"
        print "}"

        print "}"

def get_all_nodecl_structs():
    node_kind_set = set([])
    nodes = []
    for rule_name in rule_map:
        rule_rhs = rule_map[rule_name]
        for rhs in rule_rhs:
            if rhs.__class__ == NodeclStructure:
                if rhs.tree_kind not in node_kind_set:
                    node_kind_set.add(rhs.tree_kind)
                    nodes.append(rhs)
    return nodes

def generate_check_routines(rule_map):
    print "/* Autogenerated file. DO NOT MODIFY. */"
    print "/* Changes in nodecl-generator.py or cxx-nodecl.def will overwrite this file */"
    print ""
    print "#include \"cxx-nodecl.h\""
    print "#include \"cxx-utils.h\""
    print "#include \"cxx-exprtype.h\""
    print ""
    print "static void nodecl_check_nullable_rule(nodecl_t n, void (*fun)(nodecl_t))"
    print "{"
    print "   if (nodecl_is_null(n)) return;"
    print "   fun(n);"
    print "}"
    print "static void nodecl_check_list_rule(nodecl_t n, void (*fun)(nodecl_t))"
    print "{"
    print "   ERROR_CONDITION(!nodecl_is_list(n), \"Node must be a list\", 0);"
    print "   int num_items = 0;"
    print "   nodecl_t* list = nodecl_unpack_list(n, &num_items);"
    print "   int i;"
    print "   for (i = 0; i < num_items; i++)"
    print "   {"
    print "      fun(list[i]);"
    print "   }"
    print "   free(list);"
    print "}"
    print "static void nodecl_check_nullable_list_rule(nodecl_t n, void (*fun)(nodecl_t))"
    print "{"
    print "   if (nodecl_is_null(n)) return;"
    print "   nodecl_check_list_rule(n, fun);"
    print "}"

    nodes = get_all_nodecl_structs()
    for node in nodes:
        print "static void %s(nodecl_t);" % (node.check_function_name())

    for rule_name in rule_map:
        print "static void %s(nodecl_t);" % ( RuleRef(rule_name).check_function_name() )
    print ""
    for node in nodes:
        node.function_check_code()
    for rule_name in rule_map:
        RuleRef(rule_name).function_check_code()

    print "void nodecl_check_tree(AST a)"
    print "{"
    print     "nodecl_check_nodecl(_nodecl_wrap(a));"
    print "}"
    

def from_underscore_to_camel_case(x):
    result = ''
    previous_is_underscore = True
    for c in x:
        if c == '_' or c == '-':
            previous_is_underscore = True
        elif previous_is_underscore:
            previous_is_underscore = False
            result += c.upper()
        else:
            result += c.lower()
    return result


def get_all_class_names(rule_map):
    classes = set([])
    for rule_name in rule_map:
        rule_rhs = rule_map[rule_name]
        for rhs in rule_rhs:
            if rhs.__class__ == NodeclStructure:
                classes.add(from_underscore_to_camel_case(rhs.tree_kind[len(NODECL_PREFIX):]))
    return classes

def get_all_class_names_and_children_names(rule_map):
    result = []
    classes_set = set([])
    for rule_name in rule_map:
        rule_rhs = rule_map[rule_name]
        for rhs in rule_rhs:
            if rhs.__class__ == NodeclStructure:
                class_name = from_underscore_to_camel_case(rhs.tree_kind[len(NODECL_PREFIX):]);
                if class_name not in classes_set:
                    classes_set.add(class_name)
                    subtrees = map(lambda x : x[0], rhs.subtrees)
                    result.append((class_name, subtrees, rhs.tree_kind, rhs))
    return result

def generate_nodecl_classes_fwd_decls(rule_map):
    classes = get_all_class_names(rule_map)
    for class_name in classes:
        print "class %s;" % (class_name)

def generate_visitor_class_header(rule_map):
    print "/* Autogenerated file. DO NOT MODIFY. */"
    print "/* Changes in nodecl-generator.py or cxx-nodecl.def will overwrite this file */"
    print "#ifndef TL_NODECL_VISITOR_HPP"
    print "#define TL_NODECL_VISITOR_HPP"
    print ""
    print "#include <tl-objectlist.hpp>"
    print "#include <tl-nodecl.hpp>"
    print "#include \"cxx-utils.h\""
    print ""
    print "namespace Nodecl {"
    print ""
    classes = get_all_class_names(rule_map)
    print "template <typename _Ret>"
    print "class BaseNodeclVisitor;"
    print "template <>"
    print "class BaseNodeclVisitor<void>;"
    print ""
    print "template <typename _Ret>"
    print "class BaseNodeclVisitor"
    print "{"
    print "   public:"
    print "     typedef TL::ObjectList<_Ret> Ret;"
    print "     Ret walk(const NodeclBase&); /* If you override this member function you will be fired */"
    for class_name in classes:
        print "     virtual Ret visit(const Nodecl::%s &) = 0;" % (class_name)
    print "   virtual ~BaseNodeclVisitor() { }"
    print "};"
    print "template <>"
    print "class BaseNodeclVisitor<void>"
    print "{"
    print "   public:"
    print "     typedef void Ret;"
    print "     Ret walk(const NodeclBase&); /* If you override this member function you will be fired */"
    for class_name in classes:
        print "     virtual Ret visit(const Nodecl::%s &) = 0;" % (class_name)
    print "   virtual ~BaseNodeclVisitor() { }"
    print "};"
    print "template <typename _Ret>"
    print "class NodeclVisitor : public BaseNodeclVisitor<_Ret>"
    print "{"
    print "   public:"
    print "     typedef typename BaseNodeclVisitor<_Ret>::Ret Ret;"
    print "   virtual Ret unhandled_node(const Nodecl::NodeclBase &) { return Ret(); }"
    for class_name in classes:
        print "     virtual Ret visit(const Nodecl::%s & n) { return this->unhandled_node(n); }" % (class_name)
    print "   virtual ~NodeclVisitor() { }"
    print "};"
    print "template <typename _Ret>"
    print "class ExhaustiveVisitor : public NodeclVisitor<_Ret>"
    print "{"
    print "public:"
    print "     typedef typename BaseNodeclVisitor<_Ret>::Ret Ret;"
    classes_and_children = get_all_class_names_and_children_names(rule_map)
    for (class_name, children_name, tree_kind, nodecl_class) in classes_and_children:
         print "     virtual Ret visit_pre(const Nodecl::%s & n) { return Ret(); }" % (class_name)
         print "     virtual Ret visit_post(const Nodecl::%s & n) { return Ret(); }" % (class_name)
         print "     virtual Ret visit(const Nodecl::%s & n)" % (class_name)
         print "     {"
         print "        this->visit_pre(n);"
         child_num = 0
         for child_name in children_name:
              print "        this->walk(n.get_%s());" % (child_name)
              child_num = child_num + 1
         print "        this->visit_post(n);"
         print "     }"
    print "};"
    print ""

    print "template <typename _Ret>"
    print "typename BaseNodeclVisitor<_Ret>::Ret BaseNodeclVisitor<_Ret>::walk(const NodeclBase& n)"
    print "{"
    print """
    if (n.is_null())
        return Ret();
    switch ((int)n.get_kind())
    {
        case AST_NODE_LIST: { Ret result; AST tree = nodecl_get_ast(n._n); AST it; for_each_element(tree, it) { AST elem = ASTSon1(it);
NodeclBase nb(::_nodecl_wrap(elem)); result.append(this->walk(nb)); } return result; break; }
"""
    node_kind = set([])
    for rule_name in rule_map:
        rule_rhs = rule_map[rule_name]
        for rhs in rule_rhs:
            if rhs.__class__ == NodeclStructure:
                node_kind.add((rhs.tree_kind, from_underscore_to_camel_case(rhs.tree_kind[len(NODECL_PREFIX):].lower())))
    for node in node_kind:
        print "       case %s: { return this->visit(static_cast<const Nodecl::%s &>(n)); break; }" % (node[0], node[1])
    print """
       default:
           { internal_error("Unexpected tree kind '%s'\\n", ast_print_node_type(n.get_kind())); }
    }

"""
    print "}"
    print "} /* namespace Nodecl */"
    print "#endif"

def generate_visitor_class_impl(rule_map):
    print "#ifndef TL_NODECL_VISITOR_CPP"
    print "#define TL_NODECL_VISITOR_CPP"
    print "/* Autogenerated file. DO NOT MODIFY. */"
    print "/* Changes in nodecl-generator.py or cxx-nodecl.def will overwrite this file */"
    print "#include <tl-nodecl-visitor.hpp>"
    print "#include <tl-nodecl.hpp>"
    print "#include \"cxx-utils.h\""
    print ""
    print "namespace Nodecl {"
    print ""
    print "BaseNodeclVisitor<void>::Ret BaseNodeclVisitor<void>::walk(const NodeclBase& n)"
    print "{"
    print """
    if (n.is_null())
        return;
    switch ((int)n.get_kind())
    {
        case AST_NODE_LIST: { AST tree = nodecl_get_ast(n._n); AST it; for_each_element(tree, it) { AST elem = ASTSon1(it);
NodeclBase nb(::_nodecl_wrap(elem)); this->walk(nb); } break; }
"""
    node_kind = set([])
    for rule_name in rule_map:
        rule_rhs = rule_map[rule_name]
        for rhs in rule_rhs:
            if rhs.__class__ == NodeclStructure:
                node_kind.add((rhs.tree_kind, from_underscore_to_camel_case(rhs.tree_kind[len(NODECL_PREFIX):].lower())))
    for node in node_kind:
        print "       case %s: { this->visit(static_cast<const Nodecl::%s &>(n)); break; }" % (node[0], node[1])
    print """
       default:
           { internal_error("Unexpected tree kind '%s'\\n", ast_print_node_type(n.get_kind())); }
    }

"""
    print "}"
    print "} /* namespace Nodecl */"
    print "#endif // TL_NODECL_VISITOR_CPP"

def generate_nodecl_classes_base(rule_map):
   print "/* Autogenerated file. DO NOT MODIFY. */"
   print "/* Changes in nodecl-generator.py or cxx-nodecl.def will overwrite this file */"
   print "#ifndef TL_NODECL_HPP"
   print "#define TL_NODECL_HPP"
   print ""
   print "#include \"tl-nodecl-base.hpp\""
   print "#include <string>"
   print "#include <sstream>"
   
   print "namespace Nodecl {"

   classes_and_children = get_all_class_names_and_children_names(rule_map)
   for (class_name, children_name, tree_kind, nodecl_class) in classes_and_children:
       print "class %s : public NodeclBase" % (class_name)
       print "{"
       print "    private:"
       print "       static const int _kind = ::%s;" % (tree_kind)
       print "       friend class NodeclBase;"
       print "    public:"
       print "    %s() : NodeclBase() { }" %(class_name)
       print "    %s(const nodecl_t& a) : NodeclBase(a) { }" %(class_name)
       print ""

       factory_parameters = []
       for child_name in children_name:
           factory_parameters.append("Nodecl::NodeclBase child_%s" % (child_name))
       if nodecl_class.needs_symbol:
           factory_parameters.append("TL::Symbol symbol");
       if nodecl_class.needs_type:
           factory_parameters.append("TL::Type type");
       if nodecl_class.needs_text:
           factory_parameters.append("const std::string& text");
       if nodecl_class.needs_cval:
           factory_parameters.append("const_value_t* cval");
       if nodecl_class.needs_template_parameters:
           factory_parameters.append("template_parameter_list_t* template_parameters");
       if nodecl_class.needs_decl_context:
           factory_parameters.append("TL::Scope scope");

       factory_parameters.append("const std::string &filename = \"\"")
       factory_parameters.append("int line = 0")

       print "    // Factory method"
       print "    static %s make(%s);" % (class_name, string.join(factory_parameters, ", "))
       print ""

       if children_name:
            print "    // Children getters "
       child_num = 0
       for child_name in children_name:
            print "    NodeclBase get_%s() const { return NodeclBase(nodecl_get_child(_n, %d)); } " % (child_name, child_num)
            child_num = child_num + 1
       print "};"
   print ""
   print "} /* namespace Nodecl */"
   print "#endif"

def generate_nodecl_classes_specs(rule_map):
   print "/* Autogenerated file. DO NOT MODIFY. */"
   print "/* Changes in nodecl-generator.py or cxx-nodecl.def will overwrite this file */"
   print "#include \"cxx-utils.h\""
   print "#include \"tl-nodecl.hpp\""
   print ""
   print "namespace Nodecl {"
   classes_and_children = get_all_class_names_and_children_names(rule_map)
   for (class_name, children_name, tree_kind, nodecl_class) in classes_and_children:

       factory_parameters = []
       factory_arguments = []
       for child_name in children_name:
           factory_parameters.append("Nodecl::NodeclBase child_%s" % (child_name))
           factory_arguments.append("child_%s.get_internal_nodecl()" % (child_name))
       if nodecl_class.needs_symbol:
           factory_parameters.append("TL::Symbol symbol");
           factory_arguments.append("symbol.get_internal_symbol()");
       if nodecl_class.needs_type:
           factory_parameters.append("TL::Type type");
           factory_arguments.append("type.get_internal_type()");
       if nodecl_class.needs_text:
           factory_parameters.append("const std::string& text");
           factory_arguments.append("text.c_str()");
       if nodecl_class.needs_cval:
           factory_parameters.append("const_value_t* cval");
           factory_arguments.append("cval");
       if nodecl_class.needs_template_parameters:
           factory_parameters.append("template_parameter_list_t* template_parameters");
           factory_arguments.append("template_parameters");
       if nodecl_class.needs_decl_context:
           factory_parameters.append("TL::Scope scope");
           factory_arguments.append("scope.get_decl_context()");

       factory_parameters.append("const std::string &filename")
       factory_arguments.append("filename.c_str()");

       factory_parameters.append("int line")
       factory_arguments.append("line");

       nodecl_make_name = "nodecl_make_%s" % ((nodecl_class.tree_kind[len(NODECL_PREFIX):]).lower())

       print "%s %s::make(%s)" % (class_name, class_name, string.join(factory_parameters, ", "))
       print "{"
       print "    return ::%s(%s);" % (nodecl_make_name, string.join(factory_arguments, ", "))
       print "}"

   print "} /* namespace Nodecl */"

def generate_routines_header(rule_map):
   print "/* Autogenerated file. DO NOT MODIFY. */"
   print "/* Changes in nodecl-generator.py or cxx-nodecl.def will overwrite this file */"
   print "#ifndef CXX_NODECL_OUTPUT_H"
   print "#define CXX_NODECL_OUTPUT_H"
   print ""
   print "#include \"cxx-macros.h\""
   print "#include \"cxx-nodecl.h\""
   print "#include \"cxx-scope-fwd.h\""
   print "#include \"cxx-type-fwd.h\""
   print "#include \"cxx-cexpr-fwd.h\""
   print ""
   print "MCXX_BEGIN_DECLS"
   print ""

   for i in range(1, 7):
        params = map(lambda x : "nodecl_t element%d" % (x) , range(0, i))
        print "nodecl_t nodecl_make_list_%d(%s);" % (i, string.join(params, ", "))
   print ""
   classes = {}
   for rule_name in rule_map:
       rule_rhs = rule_map[rule_name]
       for rhs in rule_rhs:
           if rhs.__class__ == NodeclStructure:
               classes[(rhs.tree_kind[len(NODECL_PREFIX):]).lower()] = rhs
   for (key, rhs_rule) in classes.iteritems() :
       param_list_nodecl = map(lambda x : "nodecl_t", rhs_rule.subtrees)
       if rhs_rule.needs_symbol:
           param_list_nodecl.append("scope_entry_t*");
       if rhs_rule.needs_type:
           param_list_nodecl.append("type_t*");
       if rhs_rule.needs_text:
           param_list_nodecl.append("const char*");
       if rhs_rule.needs_cval:
           param_list_nodecl.append("const_value_t*");
       if rhs_rule.needs_template_parameters:
           param_list_nodecl.append("template_parameter_list_t*");
       if rhs_rule.needs_decl_context:
           param_list_nodecl.append("decl_context_t");
       param_list_nodecl.append("const char* filename");
       param_list_nodecl.append("int line");

       print "nodecl_t nodecl_make_%s(%s);" % (key, string.join(param_list_nodecl, ", "))
   print ""
   print "MCXX_END_DECLS"
   print ""
   print "#endif // CXX_NODECL_OUTPUT_H"
# print "key %s -> value %s" % (repr(key), repr(value))


def generate_routines_impl(rule_map):
   print "/* Autogenerated file. DO NOT MODIFY. */"
   print "/* Changes in nodecl-generator.py or cxx-nodecl.def will overwrite this file */"
   print "#include \"cxx-nodecl-output.h\""
   print "#include \"cxx-exprtype.h\""
   print "#include \"cxx-utils.h\""
   print "#include <stdlib.h>"
   print ""
   for i in range(2, 7):
        params = map(lambda x : "nodecl_t element%d" % (x) , range(0, i))
        args = map(lambda x : "element%d" % (x) , range(0, i))
        print "nodecl_t nodecl_make_list_%d(%s)" % (i, string.join(params, ", "))
        print "{"
        print "  return nodecl_append_to_list(nodecl_make_list_%d(%s), %s);" % (i-1, string.join(args[:-1], ", "), args[-1])
        print "}"

   classes = {}
   for rule_name in rule_map:
       rule_rhs = rule_map[rule_name]
       for rhs in rule_rhs:
           if rhs.__class__ == NodeclStructure:
               classes[(rhs.tree_kind[len(NODECL_PREFIX):]).lower()] = rhs
   for (key, rhs_rule) in classes.iteritems() :
       param_list_nodecl = []
       param_name_list = []
       for item in rhs_rule.subtrees:
           i = 0
           pattern = item[1].replace("-", "_") + "_%d"
           while (pattern % i) in param_name_list:
               i = i + 1
           param_name = pattern % i
           param_name_list.append(param_name)
           param_list_nodecl.append("nodecl_t %s" % (param_name))
       if rhs_rule.needs_symbol:
           param_list_nodecl.append("scope_entry_t* symbol");
       if rhs_rule.needs_type:
           param_list_nodecl.append("type_t* type");
       if rhs_rule.needs_text:
           param_list_nodecl.append("const char* text");
       if rhs_rule.needs_cval:
           param_list_nodecl.append("const_value_t* cval");
       if rhs_rule.needs_template_parameters:
           param_list_nodecl.append("template_parameter_list_t* template_parameters");
       if rhs_rule.needs_decl_context:
           param_list_nodecl.append("decl_context_t decl_context");
       param_list_nodecl.append("const char* filename");
       param_list_nodecl.append("int line");
       if not param_list_nodecl:
           raise Exception("Empty list!")

       print "nodecl_t nodecl_make_%s(%s)" % (key, string.join(param_list_nodecl, ", "))
       print "{"
       # Inline check
       i = 0
       for subrule in rhs_rule.subtrees:
          subrule_ref = RuleRef(subrule[1])
          first_set = RuleRef(subrule_ref.canonical_rule()).first();

          print "{"
          print "nodecl_t checked_tree = %s;" % (param_name_list[i])
          if not subrule_ref.is_nullable():
              print "if (nodecl_is_null(checked_tree))"
              print "{"
              print "  internal_error(\"Null node not allowed in node %d nodecl_make_%s. Location: %%s:%%d\\n\", filename, line);" % (i, key)
              print "}"
          if subrule_ref.is_nullable():
             print "if (!nodecl_is_null(checked_tree))"
             print "{"
          if subrule_ref.is_seq():
             print " if (!nodecl_is_list(checked_tree))"
             print " {"
             print "  internal_error(\"Node must be a list in node %d of nodecl_make_%s. Location: %%s:%%d\\n\", filename, line);" % (i, key)
             print " }"
             print "int i, num_items = 0;"
             print "nodecl_t* list_items = nodecl_unpack_list(checked_tree, &num_items);"
             print "for (i = 0; i < num_items; i++)"
             print "{"
             print     "checked_tree = list_items[i];"
          checks = map(lambda x : "(nodecl_get_kind(checked_tree) != %s)" % (x), first_set)
          print "if (%s)" % (string.join(checks, "\n&& "))
          print "{"
          print "  internal_error(\"Invalid node %d of type %%s in nodecl_make_%s. Location: %%s:%%d\\n\", ast_print_node_type(nodecl_get_kind(checked_tree)), filename, line);" % (i, key)
          print "}"
          if subrule_ref.is_seq():
             print "}"
             print "free(list_items);"
          if subrule_ref.is_nullable():
             print "}"
          i = i + 1
          print "}"

       # Build the node
       print "  nodecl_t result = nodecl_null();"
       num_children = len(rhs_rule.subtrees)
       if num_children == 0:
          print "  result.tree = ASTLeaf(%s, filename, line, NULL);" % (rhs_rule.tree_kind)
       else:
          print "  result.tree = ASTMake%d(%s, %s, filename, line, NULL);" % (num_children, rhs_rule.tree_kind, \
                 string.join(map(lambda x : x + ".tree", param_name_list), ", "));

       if rhs_rule.needs_symbol:
           print "  if (symbol == NULL) internal_error(\"Node requires a symbol. Location: %s:%d\", filename, line);"
           print "  nodecl_set_symbol(result, symbol);"
       if rhs_rule.needs_type:
           print "  if (type == NULL) internal_error(\"This node requires a type. Location: %s:%d\", filename, line);"
           print "  nodecl_set_type(result, type);"
       if rhs_rule.needs_text:
           print "  if (text == NULL) internal_error(\"This node requires a text. Location: %s:%d\", filename, line);"
           print "  nodecl_set_text(result, text);"
       if rhs_rule.needs_cval:
           print "  if (cval == NULL) internal_error(\"This node requires a constant value. Location: %s:%d\", filename, line);"
           print "  nodecl_set_constant(result, cval);"
       if rhs_rule.needs_template_parameters:
           print "  nodecl_set_template_parameters(result, template_parameters);"
       if rhs_rule.needs_decl_context:
           print "  nodecl_set_decl_context(result, decl_context);"

       print "  return result;"
       print "}"
       print ""

def generate_c_visitor_decl(rule_map):
    print "/* Autogenerated file. DO NOT MODIFY. */"
    print "/* Changes in nodecl-generator.py or cxx-nodecl.def will overwrite this file */"
    print """
#ifndef CXX_NODECL_VISITOR_H
#define CXX_NODECL_VISITOR_H

#include "cxx-macros.h"
#include "cxx-nodecl-decls.h"

MCXX_BEGIN_DECLS

struct nodecl_external_visitor_tag;
typedef struct nodecl_external_visitor_tag nodecl_external_visitor_t;

struct nodecl_external_visitor_tag
{
"""
    node_kind = set([])
    for rule_name in rule_map:
        rule_rhs = rule_map[rule_name]
        for rhs in rule_rhs:
            if rhs.__class__ == NodeclStructure:
                node_kind.add(rhs.tree_kind[len(NODECL_PREFIX):].lower())
    for node in node_kind:
        print "void (*visit_%s)(nodecl_external_visitor_t*, nodecl_t a);" % ( node )

    print """
};

void nodecl_walk(nodecl_external_visitor_t* external_visitor, nodecl_t node);
void nodecl_init_walker(nodecl_external_visitor_t* external_visitor, void (*default_visit)(nodecl_external_visitor_t*, nodecl_t));

#define NODECL_VISITOR_FUN(_x) (void (*)(nodecl_external_visitor_t* visitor, nodecl_t a))(_x)
#define NODECL_VISITOR(_x) ((nodecl_external_visitor_t*)(_x))
#define NODECL_WALK(v, tree) nodecl_walk(NODECL_VISITOR(v), tree)

MCXX_END_DECLS

#endif // CXX_NODECL_VISITOR_H
"""

def generate_c_visitor_def(rule_map):
    print "/* Autogenerated file. DO NOT MODIFY. */"
    print "/* Changes in nodecl-generator.py or cxx-nodecl.def will overwrite this file */"
    print """
#include "cxx-nodecl-visitor.h"
#include "cxx-utils.h"

void nodecl_walk(nodecl_external_visitor_t* external_visitor, nodecl_t n)
{
    AST tree = nodecl_get_ast(n);
    if (tree == NULL)
        return;
    switch (ASTType(tree))
    {
        case AST_NODE_LIST: { AST it; for_each_element(tree, it) { AST elem = ASTSon1(it); nodecl_walk(external_visitor, _nodecl_wrap(elem)); } break; }
"""
    node_kind = set([])
    for rule_name in rule_map:
        rule_rhs = rule_map[rule_name]
        for rhs in rule_rhs:
            if rhs.__class__ == NodeclStructure:
                node_kind.add((rhs.tree_kind, rhs.tree_kind[len(NODECL_PREFIX):].lower()))
    for node in node_kind:
        print "       case %s: { if (external_visitor->visit_%s != NULL) external_visitor->visit_%s(external_visitor, n); break; }" % (node[0], node[1], node[1])
    print """
       default:
           { internal_error("Unexpected tree kind '%s'\\n", ast_print_node_type(ASTType(tree))); }
    }
}

void nodecl_init_walker(nodecl_external_visitor_t* external_visitor, void (*default_visitor)(nodecl_external_visitor_t*, nodecl_t))
{
"""
    for node in node_kind:
        print "    external_visitor->visit_%s = default_visitor;" % (node[1])
    print """
}
"""

def generate_asttypes(rule_map):
    node_kind = set([])
    for rule_name in rule_map:
        rule_rhs = rule_map[rule_name]
        for rhs in rule_rhs:
            if rhs.__class__ == NodeclStructure:
                node_kind.add(rhs.tree_kind)
    l = list(node_kind)
    l.sort()
    for kind_name in l:
        print kind_name

# MAIN

op_mode = "check_routines"

f = open(sys.argv[1])

if len(sys.argv) > 2:
    op_mode = sys.argv[2]

rule_map = parse_rules(f)

if op_mode == "check_routines":
    generate_check_routines(rule_map)
elif op_mode == "generation_routines_header":
    generate_routines_header(rule_map)
elif op_mode == "generation_routines_impl":
    generate_routines_impl(rule_map)
elif op_mode == "cxx_visitor_decl":
    generate_visitor_class_header(rule_map)
elif op_mode == "cxx_visitor_impl":
    generate_visitor_class_impl(rule_map)
elif op_mode == "cxx_nodecl_class_header":
    generate_nodecl_classes_base(rule_map)
elif op_mode == "cxx_nodecl_class_impl":
    generate_nodecl_classes_specs(rule_map)
elif op_mode == "c_visitor_decl":
    generate_c_visitor_decl(rule_map)
elif op_mode == "c_visitor_def":
    generate_c_visitor_def(rule_map)
elif op_mode == "asttype_nodecl":
    generate_asttypes(rule_map)
else:    
    raise Exception("Invalid op_mode %s" % (op_mode))

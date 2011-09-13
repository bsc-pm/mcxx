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
                                needs_text, needs_cval, needs_template_parameters) )
                else:
                    rule_map[rule_name].append( NodeclStructure(tree_ast, [], needs_symbol, needs_type, \
                                needs_text, needs_cval, needs_template_parameters) )
            else:
                rule_map[rule_name].append( RuleRef(rhs) )
    return rule_map


class Variable:
    pass

class NodeclStructure(Variable):
    def __init__(self, tree_kind, subtrees, needs_symbol, needs_type, needs_text, needs_cval, needs_template_parameters):
        self.tree_kind = tree_kind
        self.subtrees = subtrees
        self.needs_symbol = needs_symbol
        self.needs_type = needs_type
        self.needs_text = needs_text
        self.needs_cval = needs_cval
        self.needs_template_parameters = needs_template_parameters
    def is_nullable(self, already_seen = []):
        return False
    def first(self, already_seen = []) :
        return set([self.tree_kind])
    def check_code(self, tree_expr):
        print "case %s :" % (self.tree_kind)
        print "{"
        if (self.needs_symbol):
           print "   ERROR_CONDITION(nodecl_get_symbol(%s) == NULL, \"Tree lacks a symbol\", 0);" % (tree_expr)
        if (self.needs_type):
           print "   ERROR_CONDITION(nodecl_get_type(%s) == NULL, \"Tree lacks a type\", 0);" % (tree_expr)
        if (self.needs_text):
           print "   ERROR_CONDITION(nodecl_get_text(%s) == NULL, \"Tree lacks an associated text\", 0);" % (tree_expr)
        if (self.needs_cval):
           print "   ERROR_CONDITION(nodecl_get_constant(%s) == NULL, \"Tree lacks a constant value\", 0);" % (tree_expr)
        # We do not check template parameters
        i = 0
        for subtree in self.subtrees:
           (rule_label, rule_ref) = subtree

           current_rule = RuleRef(rule_ref)

           if current_rule.is_nullable():
               print "if (!nodecl_is_null(nodecl_get_child(%s, %d)))" % (tree_expr, i) 
               print "{"
           else:
               print "ERROR_CONDITION(nodecl_is_null(nodecl_get_child(%s, %d)), \"Tree cannot be NULL!\", 0);" % (tree_expr, i)

           if current_rule.is_seq():
               print "ERROR_CONDITION(!nodecl_is_list(nodecl_get_child(%s, %d)), \"List node required here but got %%s\", ast_print_node_type(nodecl_get_kind(nodecl_get_child(%s, %d))));" % (tree_expr, i, tree_expr, i)
               current_rule.check_code("nodecl_get_child(%s, %d)" % (tree_expr, i))
           else:
               print "   switch (nodecl_get_kind(nodecl_get_child(%s, %d)))" % (tree_expr, i)
               print "   {"
               print "        // rule -> %s" % (rule_ref)
               current_rule.check_code("nodecl_get_child(%s, %d)" % (tree_expr, i))
               print "       default:"
               print "       {"
               first_set = current_rule.first()
               if current_rule.is_seq():
                  print "           internal_error(\"Invalid tree kind '%%s' expecting an AST_NODE_LIST\", ast_print_node_type(nodecl_get_kind(nodecl_get_child(%s, %d))));" \
                                      % (tree_expr, i)
               else:
                  print "           internal_error(\"Invalid tree kind '%%s' expecting one of %s\", ast_print_node_type(nodecl_get_kind(nodecl_get_child(%s, %d))));" \
                      % (string.join(first_set, " or "), tree_expr, i)
               print "          break;"
               print "       }"
               print "   }"

           if (current_rule.is_nullable()):
               print "}"

           i = i + 1
        print "  break;"
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
        already_seen.append(rule_ref)
        rule_set = rule_map[rule_ref]
        s = set([])
        for rhs in rule_set:
            s = s.union(rhs.first(already_seen))
        return s
    def check_code(self, tree_expr):
        (rule_ref, rule_ref_c, is_seq, is_opt) = self.normalize_rule_name(self.rule_ref)
        first_set = self.first()
        if not first_set:
            raise Exception("First set can't be empty!")
        if is_seq:
           print "{"
           print "nodecl_t list = %s;" % (tree_expr)
           print "int i, num_items = 0;"
           print "nodecl_t* list_items = nodecl_unpack_list(list, &num_items);"
           print "for (i = 0; i < num_items; i++)"
           print "{"
           print "   nodecl_t e = list_items[i];"
           print "   switch (nodecl_get_kind(e))"
           print "   {"
           for first_item in first_set:
                 print "        case %s : " % (first_item) 
           print "        {"
           print "               nodecl_check_tree_%s(e);" % (rule_ref_c)
           print "               break;"
           print "        }"
           print "        default:"
           print "        {"
           print "           internal_error(\"Invalid tree kind '%%s' expecting one of %s\", ast_print_node_type(nodecl_get_kind(e)));" \
               % (string.join(first_set, " or "))
           print "           break;"
           print "        }"
           print "   }"
           print "}"
           print "free(list_items);"
           print "}"
        else:
           for first_item in first_set:
                 print "        case %s : " % (first_item) 
           print "        {"
           print "               nodecl_check_tree_%s(%s);" % (rule_ref_c, tree_expr)
           print "               break;"
           print "        }"

def generate_check_routines_rec(rule_map, rule_name):
    rule_info = rule_map[rule_name]
    rule_name_c = rule_name.replace("-", "_");
    print "static void nodecl_check_tree_%s(nodecl_t n)" % (rule_name_c)
    print "{"
    print "   ERROR_CONDITION(nodecl_is_null(n), \"Invalid null tree\", 0);"
    print "   switch (nodecl_get_kind(n))"
    print "   {"
    first_set = set([])
    for rhs in rule_info:
        rhs.check_code("n")
        first_set = first_set.union(rhs.first())
    print "     default:"
    print "     {"
    print "           internal_error(\"Invalid tree kind '%%s' expecting one of %s\", ast_print_node_type(nodecl_get_kind(n)));" \
        % (string.join(first_set, " or "))
    print "        break;"
    print "     }"
    print "   }"
    print "}"


def generate_check_routines(rule_map):
    print "/* Autogenerated file. DO NOT MODIFY. */"
    print "/* Changes in nodecl-generator.py or cxx-nodecl.def will overwrite this file */"
    print ""
    print "#include \"cxx-nodecl-decls.h\""
    print "#include \"cxx-utils.h\""
    print "#include \"cxx-exprtype.h\""
    print ""
    for rule_name in rule_map:
        rule_name_c = rule_name.replace("-", "_");
        print "static void nodecl_check_tree_%s(nodecl_t);" % (rule_name_c)
    print ""
    for rule_name in rule_map:
        generate_check_routines_rec(rule_map, rule_name)
    print "void nodecl_check_tree(nodecl_t n)"
    print "{"
    print "   nodecl_check_tree_nodecl(n);"
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
                    result.append((class_name, subtrees, rhs.tree_kind))
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
    print ""
    print "namespace Nodecl {"
    # print "   class NodeclBase;"
    # generate_nodecl_classes_fwd_decls(rule_map)
    # print ""
    classes = get_all_class_names(rule_map)
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
    print ""
    print "} /* namespace Nodecl */"
    print "#include \"tl-nodecl-visitor.cpp\""
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
    print "#endif // TL_NODECL_VISITOR_CPP"

def generate_nodecl_classes_base(rule_map):
   print "/* Autogenerated file. DO NOT MODIFY. */"
   print "/* Changes in nodecl-generator.py or cxx-nodecl.def will overwrite this file */"
   print "#ifndef TL_NODECL_HPP"
   print "#define TL_NODECL_HPP"
   print ""
   print "#include \"cxx-nodecl.h\""
   print "#include \"tl-object.hpp\""
   print "#include \"tl-symbol.hpp\""
   print "#include \"tl-type.hpp\""
   print "#include <string>"
   print "#include <sstream>"
   print "namespace Nodecl {"
   print ""
   print "template <typename Ret> class BaseNodeclVisitor;"
   print ""
   print "class NodeclBase : public TL::Object"
   print "{"
   print "  protected:"
   print "   nodecl_t _n;"
   print "  public:"
   print "    NodeclBase(const nodecl_t& n) : _n(n) { }"
   print "    node_t get_kind() const { return ::nodecl_get_kind(_n); }"
   print "    bool is_null() const { return ::nodecl_is_null(_n); }"
   print "    static NodeclBase null() { return NodeclBase(::nodecl_null()); }"
   print "    virtual ~NodeclBase() { }"
   print "    TL::Type get_type() const { return TL::Type(::nodecl_get_type(_n)); }"
   print "    TL::Symbol get_symbol() const { return TL::Symbol(::nodecl_get_symbol(_n)); }"
   print "    std::string get_text() const { return std::string(::nodecl_get_text(_n)); }"
   # TODO add const_value_t*
   print "    std::string get_filename() const { const char* c = nodecl_get_filename(_n); if (c == NULL) c = \"(null)\"; return c; }"
   print "    int get_line() const { return nodecl_get_line(_n); }"
   print "    std::string get_locus() const { std::stringstream ss; ss << this->get_filename() << \":\" << this->get_line(); return ss.str(); }"
   print "    nodecl_t get_internal_nodecl() const { return _n; }"
   print "    // Simple RTTI"
   print "    template <typename T> bool is() const { return !this->is_null() && (T::_kind == this->get_kind()); }"
   print "    template <typename T> T as() const { return T(this->_n); }"
   print "    template <typename Ret> friend class BaseNodeclVisitor;"
   print "};"
   classes_and_children = get_all_class_names_and_children_names(rule_map)
   for (class_name, children_name, tree_kind) in classes_and_children:
       print "class %s : public NodeclBase" % (class_name)
       print "{"
       print "    private:"
       print "       static const int _kind = ::%s;" % (tree_kind)
       print "       friend class NodeclBase;"
       print "    public:"
       print "    %s(const nodecl_t& a) : NodeclBase(a) { }" %(class_name)
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
   classes = {}
   for rule_name in rule_map:
       rule_rhs = rule_map[rule_name]
       for rhs in rule_rhs:
           if rhs.__class__ == NodeclStructure:
               classes[from_underscore_to_camel_case(rhs.tree_kind[len(NODECL_PREFIX):])] = rhs.subtrees
#   for (class_name, subtrees) in classes.iteritems():
#       print "void %s::accept(BaseNodeclVisitor& visitor)" % (class_name)
#       print "{"
#       print "    visitor.visit(*this);"
#       print "}"
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
          first_set = subrule_ref.first();

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

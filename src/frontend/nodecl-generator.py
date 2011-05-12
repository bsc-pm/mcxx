#!/usr/bin/python

import sys
import string
import re

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
            # Assume this is an AST name
            if (rhs[0:4] == "AST_"):
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
                        
                    rule_map[rule_name].append( ASTStructure(tree_ast, ast_args_3, needs_symbol, needs_type, needs_text, needs_cval) )
                else:
                    rule_map[rule_name].append( ASTStructure(tree_ast, [], needs_symbol, needs_type, needs_text, needs_cval) )
            else:
                rule_map[rule_name].append( RuleRef(rhs) )
    return rule_map


class Variable:
    pass

class ASTStructure(Variable):
    def __init__(self, tree_kind, subtrees, needs_symbol, needs_type, needs_text, needs_cval):
        self.tree_kind = tree_kind
        self.subtrees = subtrees
        self.needs_symbol = needs_symbol
        self.needs_type = needs_type
        self.needs_text = needs_text
        self.needs_cval = needs_cval
    def first(self) :
        return set([self.tree_kind])
    def check_code(self, tree_expr):
        print "case %s :" % (self.tree_kind)
        print "{"
        if (self.needs_symbol):
           print "   ERROR_CONDITION(expression_get_symbol(%s) == NULL, \"Tree lacks a symbol\", 0);" % (tree_expr)
        if (self.needs_type):
           print "   ERROR_CONDITION(expression_get_type(%s) == NULL, \"Tree lacks a type\", 0);" % (tree_expr)
        if (self.needs_text):
           print "   ERROR_CONDITION(ASTText(%s) == NULL, \"Tree lacks an associated text\", 0);" % (tree_expr)
        if (self.needs_cval):
           print "   ERROR_CONDITION(expression_get_constant(%s) == NULL, \"Tree lacks a constant value\", 0);" % (tree_expr)
        i = 0
        for subtree in self.subtrees:
           (rule_label, rule_ref) = subtree

           current_rule = RuleRef(rule_ref)

           if current_rule.is_opt():
               print "if (ASTSon%d(%s) != NULL)" % (i,tree_expr) 
               print "{"
           else:
               print "ERROR_CONDITION(ASTSon%d(%s) == NULL, \"Tree cannot be NULL!\", 0);" % (i, tree_expr)

           if current_rule.is_seq():
               print "ERROR_CONDITION(ASTType(ASTSon%d(%s)) != AST_NODE_LIST, \"List node required here but got %%s\", ast_print_node_type(ASTType(ASTSon%d(%s))));" % (i, tree_expr, i, tree_expr)
               current_rule.check_code("ASTSon%d(%s)" % (i, tree_expr))
           else:
               print "   switch (ASTType(ASTSon%d(%s)))" % (i,tree_expr)
               print "   {"
               print "        // rule -> %s" % (rule_ref)
               current_rule.check_code("ASTSon%d(%s)" % (i, tree_expr))
               print "       default:"
               print "       {"
               first_set = current_rule.first()
               if current_rule.is_seq():
                  print "           internal_error(\"Invalid tree kind '%%s' expecting an AST_NODE_LIST\", ast_print_node_type(ASTType(ASTSon%d(%s))));" \
                                      % (i, tree_expr)
               else:
                  print "           internal_error(\"Invalid tree kind '%%s' expecting one of %s\", ast_print_node_type(ASTType(ASTSon%d(%s))));" \
                      % (string.join(first_set, " or "), i, tree_expr)
               print "          break;"
               print "       }"
               print "   }"

           if (current_rule.is_opt()):
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
    def first(self) :
        (rule_ref, rule_ref_c, is_seq, is_opt) = self.normalize_rule_name(self.rule_ref)
        rule_set = rule_map[rule_ref]
        s = set([])
        for rhs in rule_set:
            s = s.union(rhs.first())
        return s
    def check_code(self, tree_expr):
        (rule_ref, rule_ref_c, is_seq, is_opt) = self.normalize_rule_name(self.rule_ref)
        first_set = self.first()
        if is_seq:
           print "AST it;"
           print "for_each_element(%s, it)" % (tree_expr)
           print "{"
           print "   AST e = ASTSon1(it);"
           print "   switch (ASTType(e))"
           print "   {"
           for first_item in first_set:
                 print "        case %s : " % (first_item) 
           print "        {"
           print "               nodecl_check_tree_%s(e);" % (rule_ref_c)
           print "               break;"
           print "        }"
           print "        default:"
           print "        {"
           print "           internal_error(\"Invalid tree kind '%%s' expecting one of %s\", ast_print_node_type(ASTType(e)));" \
               % (string.join(first_set, " or "))
           print "           break;"
           print "        }"
           print "   }"
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
    print "static void nodecl_check_tree_%s(AST a)" % (rule_name_c)
    print "{"
    print "   ERROR_CONDITION(a == NULL, \"Invalid null tree\", 0);"
    print "   switch (ASTType(a))"
    print "   {"
    first_set = set([])
    for rhs in rule_info:
        rhs.check_code("a")
        first_set = first_set.union(rhs.first())
    print "     default:"
    print "     {"
    print "           internal_error(\"Invalid tree kind '%%s' expecting one of %s\", ast_print_node_type(ASTType(a)));" \
        % (string.join(first_set, " or "))
    print "        break;"
    print "     }"
    print "   }"
    print "}"


def generate_check_routines(rule_map):
    print "/* Autogenerated file. DO NOT MODIFY. */"
    print "/* Changes in nodecl-generator.py or cxx-nodecl.def will overwrite this file */"
    print ""
    print "#include \"cxx-ast.h\""
    print "#include \"cxx-asttype.h\""
    print "#include \"cxx-utils.h\""
    print "#include \"cxx-exprtype.h\""
    print ""
    for rule_name in rule_map:
        rule_name_c = rule_name.replace("-", "_");
        print "static void nodecl_check_tree_%s(AST);" % (rule_name_c)
    print ""
    for rule_name in rule_map:
        generate_check_routines_rec(rule_map, rule_name)
    print "void nodecl_check_tree(AST a)"
    print "{"
    print "   nodecl_check_tree_nodecl(a);"
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
            if rhs.__class__ == ASTStructure:
                classes.add(from_underscore_to_camel_case(rhs.tree_kind[4:]))
    return classes

def generate_nodecl_classes_fwd_decls(rule_map):
    classes = get_all_class_names(rule_map)
    for class_name in classes:
        print "class %s;" % (class_name)

def generate_visitor_class(rule_map):
    print "/* Autogenerated file. DO NOT MODIFY. */"
    print "/* Changes in nodecl-generator.py or cxx-nodecl.def will overwrite this file */"
    print "#ifndef TL_NODECL_VISITOR_HPP"
    print "#define TL_NODECL_VISITOR_HPP"
    print ""
    print "namespace TL {"
    generate_nodecl_classes_fwd_decls(rule_map)
    print ""
    classes = get_all_class_names(rule_map)
    print "class NodeclVisitor"
    print "{"
    print "   public:"
    for class_name in classes:
        print "     virtual void visit_preorder(%s &) { }" % (class_name)
        print "     virtual void visit_postorder(%s &) { }" % (class_name)
    print "   virtual ~NodeclVisitor() { }"
    print "};"
    print ""
    print "}"
    print "#endif"

def generate_nodecl_classes_base(rule_map):
   print "/* Autogenerated file. DO NOT MODIFY. */"
   print "/* Changes in nodecl-generator.py or cxx-nodecl.def will overwrite this file */"
   print "#ifndef TL_NODECL_AST_HPP"
   print "#define TL_NODECL_AST_HPP"
   print ""
   print "#include \"tl-ast.hpp\""
   print "#include \"tl-nodecl-visitor.hpp\""
   print "namespace TL {"
   print ""
   print "class NodeclAST : public AST_t" 
   print "{"
   print "  public:"
   print "    NodeclAST(const AST_t& a) : AST_t(a) { }"
   print "    virtual void accept(NodeclVisitor& visitor) { }"
   print "    virtual ~NodeclAST() { }"
   print "};"
   classes = get_all_class_names(rule_map)
   for class_name in classes:
       print "class %s : public NodeclAST" % (class_name)
       print "{"
       print "    public:"
       print "    %s(AST_t a) : NodeclAST(a) { }" %(class_name)
       print "    virtual void accept(NodeclVisitor& visitor);"
       print "};"
   print ""
   print "}"
   print "#endif"

def generate_nodecl_classes_specs(rule_map):
   print "/* Autogenerated file. DO NOT MODIFY. */"
   print "/* Changes in nodecl-generator.py or cxx-nodecl.def will overwrite this file */"
   print "#include \"cxx-utils.h\""
   print "#include \"tl-nodecl-ast.hpp\""
   print ""
   print "namespace TL {"
   classes = {}
   for rule_name in rule_map:
       rule_rhs = rule_map[rule_name]
       for rhs in rule_rhs:
           if rhs.__class__ == ASTStructure:
               classes[from_underscore_to_camel_case(rhs.tree_kind[4:])] = rhs.subtrees
   for (class_name, subtrees) in classes.iteritems():
       print "void %s::accept(NodeclVisitor& visitor)" % (class_name)
       print "{"
       print "    visitor.visit_preorder(*this);"
       if subtrees :
               print "   ObjectList<AST_t> children(this->children());"
       i = 0;
       for subtree in subtrees:
           rule_ref = RuleRef(subtree[1])
           tree_name = "children[%d]" % (i)
           if rule_ref.is_opt():
               print "   if (%s.is_valid())" % (tree_name)
               print "   {"
               pass
           if rule_ref.is_seq():
               print "   ASTIterator it = %s.get_list_iterator();" % (tree_name)
               print "   while (!it.end())"
               print "   {"
               tree_name = "it.item()"
               pass
           print "   switch ((int)%s.internal_ast_type_())" % (tree_name)
           print "   {"
           first_set = rule_ref.first()
           for kind in first_set:
                 print "       case %s: " % (kind)
                 print "       {"
                 print "           %s t(%s);" % (from_underscore_to_camel_case(kind[4:]), tree_name)
                 print "           t.accept(visitor);"
                 print "           break;"
                 print "       }"
           print "       default:"
           print "       {"
           print "          internal_error(\"Unexpected tree %s\\n\", this->internal_ast_type().c_str());"
           print "          break;"
           print "       }"
           print "   }"
           i = i + 1         
           if rule_ref.is_seq():
               print "    it.next();"
               print "}"
           if rule_ref.is_opt():
               print "}"
       print "    visitor.visit_postorder(*this);"
       print "}"
   print "}"

def generate_routines_header(rule_map):
   print "#ifndef CXX_NODECL_OUTPUT_H"
   print "#define CXX_NODECL_OUTPUT_H"
   print ""
   print "#include \"cxx-macros.h\""
   print "#include \"cxx-nodecl-output-decls.h\""
   print "#include \"cxx-scope-decls.h\""
   print "#include \"cxx-typeutils.h\""
   print "#include \"cxx-cexpr.h\""
   print ""
   print "MCXX_BEGIN_DECLS"
   print ""
   print "nodecl_output_t nodecl_null(void);"
   print ""
   print "// list' parameter can be a 'nodecl_null()'"
   print "nodecl_output_t nodecl_append_to_list(nodecl_output_t list, nodecl_output_t element);"
   print ""
   print "// Either list1 or list2 can be 'nodecl_null()'"
   print "nodecl_output_t nodecl_concat_lists(nodecl_output_t list1, nodecl_output_t list2);"
   print ""
   classes = {}
   for rule_name in rule_map:
       rule_rhs = rule_map[rule_name]
       for rhs in rule_rhs:
           if rhs.__class__ == ASTStructure:
               classes[(rhs.tree_kind[4:]).lower()] = rhs
   for (key, rhs_rule) in classes.iteritems() :
       param_list_nodecl = map(lambda x : "nodecl_output_t", rhs_rule.subtrees)
       if rhs_rule.needs_symbol:
           param_list_nodecl.append("scope_entry_t*");
       if rhs_rule.needs_type:
           param_list_nodecl.append("type_t*");
       if rhs_rule.needs_text:
           param_list_nodecl.append("const char*");
       if rhs_rule.needs_cval:
           param_list_nodecl.append("const_value_t*");
       if not param_list_nodecl:
           param_list_nodecl = ["void"];

       print "nodecl_output_t nodecl_make_%s(%s);" % (key, string.join(param_list_nodecl, ", "))
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
   print \
"""
nodecl_output_t nodecl_null(void)
{
    nodecl_output_t result = { NULL };
    return result;
}

nodecl_output_t nodecl_concat_lists(nodecl_output_t list1, nodecl_output_t list2)
{
    if (list1.tree == NULL)
        return list2;

    if (list2.tree == NULL)
        return list1;

    if (ASTType(list1.tree) == AST_NODE_LIST
            && ASTType(list2.tree) == AST_NODE_LIST)
    {
        nodecl_output_t result;
        result.tree = ast_list_concat(list1.tree, list2.tree);
        return result;
    }

    internal_error("Invalid trees when appending two nodecl_output_t", 0);
    nodecl_output_t result = { NULL };
    return result;
}
nodecl_output_t nodecl_append_to_list(nodecl_output_t list, nodecl_output_t element)
{
    if (list.tree != NULL)
    {
        nodecl_output_t result = { ASTListLeaf(element.tree) };
        return result;
    }
    else
    {
        nodecl_output_t result = { ASTList(list.tree, element.tree) };
        return result;
    }
}
"""
   classes = {}
   for rule_name in rule_map:
       rule_rhs = rule_map[rule_name]
       for rhs in rule_rhs:
           if rhs.__class__ == ASTStructure:
               classes[(rhs.tree_kind[4:]).lower()] = rhs
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
           param_list_nodecl.append("nodecl_output_t %s" % (param_name))
       if rhs_rule.needs_symbol:
           param_list_nodecl.append("scope_entry_t* symbol");
       if rhs_rule.needs_type:
           param_list_nodecl.append("type_t* type");
       if rhs_rule.needs_text:
           param_list_nodecl.append("const char* text");
       if rhs_rule.needs_cval:
           param_list_nodecl.append("const_value_t* cval");
       if not param_list_nodecl:
           param_list_nodecl = ["void"];

       print "nodecl_output_t nodecl_make_%s(%s)" % (key, string.join(param_list_nodecl, ", "))
       print "{"
       print "  nodecl_output_t result = nodecl_null();"
       num_children = len(rhs_rule.subtrees)
       if num_children == 0:
          print "  result.tree = ASTLeaf(%s, NULL, 0, NULL);" % (rhs_rule.tree_kind)
       else:
          print "  result.tree = ASTMake%d(%s, %s, NULL, 0, NULL);" % (num_children, rhs_rule.tree_kind, \
                 string.join(map(lambda x : x + ".tree", param_name_list), ", "));

       if rhs_rule.needs_symbol:
          print "  expression_set_symbol(result.tree, symbol);"
       if rhs_rule.needs_type:
          print "  expression_set_type(result.tree, type);"
       if rhs_rule.needs_text:
          print "  ast_set_text(result.tree, text);"
       if rhs_rule.needs_cval:
          print "  expression_set_constant(result.tree, cval);"

       print "  return result;"
       print "}"
       print ""

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
    generate_visitor_class(rule_map)
elif op_mode == "cxx_nodecl_class_header":
    generate_nodecl_classes_base(rule_map)
elif op_mode == "cxx_nodecl_class_impl":
    generate_nodecl_classes_specs(rule_map)
else:    
    raise Exception("Invalid op_mode %s" % (op_mode))

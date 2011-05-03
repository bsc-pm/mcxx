#!/usr/bin/python

import sys
import string


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
    
    # for r in rule_set:
    #     print r
    # 
    # print 50 * "*"
    
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
                if ast_args :
                    ast_args_2 = map(lambda x : x.strip(), ast_args.split(","))
                    rule_map[rule_name].append( ASTStructure(tree_ast, ast_args_2, needs_symbol, needs_type, needs_text) )
                else:
                    rule_map[rule_name].append( ASTStructure(tree_ast, [], needs_symbol, needs_type, needs_text) )
            else:
                rule_map[rule_name].append( RuleRef(rhs) )
    return rule_map


class Variable:
    pass

class ASTStructure(Variable):
    def __init__(self, tree_kind, subtrees, needs_symbol, needs_type, needs_text):
        self.tree_kind = tree_kind
        self.subtrees = subtrees
        self.needs_symbol = needs_symbol
        self.needs_type = needs_type
        self.needs_text = needs_text
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
        i = 0
        for subtree in self.subtrees:
            RuleRef(subtree).check_code("ASTSon%d(%s)" % (i, tree_expr))
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
        if is_opt:
           print "if (%s != NULL)" % (tree_expr)
           print "{"
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
           print "           internal_error(\"Invalid tree kind '%s'\", ast_print_node_type(ASTType(a)));"
           print "           break;"
           print "        }"
           print "   }"
           print "}"
        else:
           print "   switch (ASTType(%s))" % (tree_expr)
           print "   {"
           for first_item in first_set:
                 print "        case %s : " % (first_item) 
           print "        {"
           print "               nodecl_check_tree_%s(%s);" % (rule_ref_c, tree_expr)
           print "               break;"
           print "        }"
           print "        default:"
           print "        {"
           print "           internal_error(\"Invalid tree kind '%s'\", ast_print_node_type(ASTType(a)));"
           print "           break;"
           print "        }"
           print "   }"
        if is_opt:
           print "}"

def generate_check_routines_rec(rule_map, rule_name):
    rule_info = rule_map[rule_name]
    rule_name_c = rule_name.replace("-", "_");
    print "static void nodecl_check_tree_%s(AST a)" % (rule_name_c)
    print "{"
    print "   ERROR_CONDITION(a == NULL, \"Invalid null tree\", 0);"
    print "   switch (ASTType(a))"
    print "   {"
    for rhs in rule_info:
        rhs.check_code("a")
    print "     default:"
    print "     {"
    print "        internal_error(\"Invalid tree kind '%s'\", ast_print_node_type(ASTType(a)));"
    print "        break;"
    print "     }"
    print "   }"
    print "}"


def generate_check_routines(rule_map):
    print "/* This file is self-generated. DO NOT MODIFY */"
    print "/* Changes to cxx-nodecl.def will update this file */"
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


# MAIN

op_mode = "check_routines"

f = open(sys.argv[1])

if len(sys.argv) > 2:
    op_mode = sys.argv[2]

rule_map = parse_rules(f)

if op_mode == "check_routines":
    generate_check_routines(rule_map)
else:    
    raise Exception("Invalid op_mode %s" % (op_mode))

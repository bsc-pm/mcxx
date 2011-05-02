#!/usr/bin/python

import sys
import string

f = open(sys.argv[1])

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

class Variable:
    def gen_code(self, rule_map):
        raise Exception("Invalid gen_code called in Variable")

class RuleName(Variable):
    def __init__(self, rule_name):
        self.rule_name = rule_name
    def __repr__(self):
        return "Rule(%s)" % repr(self.rule_name)
    def gen_code(self, rule_map):
        rule_name = self.rule_name
        is_opt = 0
        is_seq = 0
        if rule_name[-8:] == "-opt-seq":
             rule_name = rule_name[:-8]
             is_opt = 1
             is_seq = 1
        if rule_name[-4:] == "-opt":
             rule_name = rule_name[:-4]
             is_opt = 1
        if rule_name[-4:] == "-seq":
             rule_name = rule_name[:-4]
             is_seq = 1
        if rule_name not in rule_map:
            raise Exception("Invalid rule-name %s\n" % (self.rule_name))
        rule = rule_map[rule_name]
        for rhs in rule:
            rhs.gen_code(rule_map)

class ASTStructure(Variable):
    def __init__(self, tree_type, children_seq):
        self.tree_type = tree_type
        self.children_seq = children_seq
    def __repr__(self):
        return "ASTStructure(%s)" % repr(self.children_seq)
    def gen_code(self, rule_map):
        print "if (ASTType(a) == " + self.tree_type + ")"

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

for r in rule_set:
    print r

print 50 * "*"

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
            ast_args_2 = map(lambda x : x.strip(), ast_args.split(","))
            rule_map[rule_name].append( ASTStructure(tree_ast, ast_args_2) )
        else:
            rule_map[rule_name].append( RuleName(rhs) )

def print_code_map(rule_map, rule_name):
    if rule_name not in rule_map:
        raise Exception("Invalid rule-name %s\n" % (rule_name))
    rhs_set = rule_map[rule_name]
    for rhs in rhs_set:
        rhs.gen_code(rule_map)

print_code_map(rule_map, "nodecl")

#!/usr/bin/python

#  (C) Copyright 2006-2015 Barcelona Supercomputing Center
#                          Centro Nacional de Supercomputacion
#  
#  This file is part of Mercurium C/C++ source-to-source compiler.
#  
#  See AUTHORS file in the top level directory for information
#  regarding developers and contributors.
#  
#  This library is free software; you can redistribute it and/or
#  modify it under the terms of the GNU Lesser General Public
#  License as published by the Free Software Foundation; either
#  version 3 of the License, or (at your option) any later version.
#  
#  Mercurium C/C++ source-to-source compiler is distributed in the hope
#  that it will be useful, but WITHOUT ANY WARRANTY; without even the
#  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#  PURPOSE.  See the GNU Lesser General Public License for more
#  details.
#  
#  You should have received a copy of the GNU Lesser General Public
#  License along with Mercurium C/C++ source-to-source compiler; if
#  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
#  Cambridge, MA 02139, USA.

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
    
    regex_name = re.compile("\[([_A-Za-z][_*A-Za-z0-9]*)\]\s*([-_A-Za-z0-9]+)")
    rule_map = { }
    rule_map["any"] = []
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
                attributes = [
                        ("symbol", "symbol"),
                        ("type", "type"),
                        ("text", "text"),
                        ("cval", "const-value"),
                        ("template_parameters", "template-parameters"),
                        ("decl_context", "context")]

                # Mandatory values
                # This exec below performs the following statements
                #      needs_symbol = "symbol" in remaining_flags
                #      needs_type = "type" in remaining_flags
                #      ...
                for (base_var_name, attr) in attributes:
                    statement = "needs_%s = \"%s\" in remaining_flags" % (base_var_name, attr)
                    exec statement

                # Optional values
                # This exec below perfoms the following statements
                #      may_have_symbol = "symbol-opt" in remaining_flags
                #      may_have_type = "type-opt" in remaining_flags
                #      ...
                for (base_var_name, attr) in attributes:
                    statement = "may_have_%s = \"%s-opt\" in remaining_flags" % (base_var_name, attr)
                    exec statement

                # Module
                module_name = "base"
                for flag in remaining_flags:
                    if flag[0:len("module")] == "module":
                        i = flag.find("(")
                        if i < 0:
                            raise Exception("invalid syntax, expecting (")
                        j = find_matching_parentheses(flag[i:])
                        module_name = flag[i+1: i + j].strip()

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
                    nodecl_structure = NodeclStructure(tree_ast, ast_args_3)
                    # , needs_symbol, needs_type, \
                    #             needs_text, needs_cval, needs_template_parameters, needs_decl_context) )
                else:
                    nodecl_structure = NodeclStructure(tree_ast, [])
                    #needs_symbol, needs_type, \
                    #needs_text, needs_cval, needs_template_parameters, needs_decl_context) )

                for (base_var_name, attr) in attributes:
                    setattr(nodecl_structure, "needs_%s" % (base_var_name), eval("needs_%s" % (base_var_name)))
                    setattr(nodecl_structure, "may_have_%s" % (base_var_name), eval("may_have_%s" % (base_var_name)))

                nodecl_structure.module_name = module_name
                rule_map[rule_name].append( nodecl_structure )
            else:
                rule_map[rule_name].append( RuleRef(rhs) )

    for current_rule_name in rule_map:
        can_be_seq = 1
        can_be_opt = 1
        has_to_be_seq = 0
        has_to_be_opt = 0
        for rhs in rule_map[current_rule_name]:
            if rhs.__class__ == NodeclStructure:
                if has_to_be_seq:
                    raise Exception("inconsistent RHS: should generate a list, not a single node")
                if has_to_be_opt:
                    raise Exception("inconsistent RHS: should generate an optional node, not a single node")
                can_be_seq = 0
                can_be_opt = 0
            elif rhs.__class__ == RuleRef:
                current_is_seq = rhs.rule_ref.find("-seq") > 0
                current_is_opt = rhs.rule_ref.find("-opt") > 0
                if not can_be_seq and current_is_seq:
                    raise Exception("inconsistent RHS: cannot generate a list")
                if not can_be_opt and current_is_opt:
                    raise Exception("inconsistent RHS: cannot generate an optional node")
                if not current_is_seq and has_to_be_seq:
                    raise Exception("inconsistent RHS: must generate a list")
                if not current_is_opt and has_to_be_opt:
                    raise Exception("inconsistent RHS: must generate an optional node")
                if current_is_seq:
                    has_to_be_seq = 1
                if current_is_opt:
                    has_to_be_opt = 1
            else:
                raise Exception("invalid kind of node")
    return rule_map


class Variable:
    pass

class NodeclStructure(Variable):
    def __init__(self, tree_kind, subtrees):
        self.tree_kind = tree_kind
        self.subtrees = subtrees
        self.module_name = "base"

    def name_to_underscore(self):
        return self.tree_kind.replace("-", "_").replace("*", "_")
    def name_to_underscore_lowercase(self):
        return self.name_to_underscore().lower()

    def is_nullable(self, already_seen = []):
        return False
    def first(self, already_seen = []) :
        return set([self.name_to_underscore()])

    def base_name(self):
        return self.tree_kind[len(NODECL_PREFIX):]
    def base_name_to_underscore(self):
        return self.base_name().replace("-", "_").replace("*", "_")
    def base_name_to_underscore_lowercase(self):
        return self.base_name_to_underscore().lower()

    def check_function_name(self):
        return "nodecl_check_%s" % (self.base_name_to_underscore())
    def c_name(self):
        return "nodecl_" + self.base_name_to_underscore_lowercase()

    def call_to_check(self, tree_name):
        return "%s(%s);" % (self.check_function_name(), tree_name);
    def function_check_code(self):
        print "static void %s(nodecl_t n)" % (self.check_function_name())
        print "{"
        print "ERROR_CONDITION(nodecl_is_null(n), \"Node is null\", 0);"
        print "ERROR_CONDITION(nodecl_get_kind(n) != %s, \"Invalid node\", 0);" % self.name_to_underscore()
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
        rule_ref_c = rule_ref.replace("-", "_").replace("*", "_")
        return (rule_ref, rule_ref_c, is_seq, is_opt) 
    def canonical_rule(self):
        (rule_ref, rule_ref_c, is_seq, is_opt) = self.normalize_rule_name(self.rule_ref)
        return rule_ref
    def rule_c_name(self):
        (rule_ref, rule_ref_c, is_seq, is_opt) = self.normalize_rule_name(self.rule_ref)
        return rule_ref_c
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
        return "nodecl_check_%s" % (self.rule_c_name())
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
                if rhs.name_to_underscore() not in node_kind_set:
                    node_kind_set.add(rhs.name_to_underscore())
                    nodes.append(rhs)
    return nodes

def generate_check_routines(rule_map):
    print "/* Autogenerated file. DO NOT MODIFY. */"
    print "/* Changes in nodecl-generator.py or cxx-nodecl.def will overwrite this file */"
    print ""
    print "#include \"cxx-nodecl.h\""
    print "#include \"cxx-utils.h\""
    print "#include \"cxx-exprtype.h\""
    print "#include \"mem.h\""
    print ""
    print "static inline void nodecl_check_nullable_rule(nodecl_t n, void (*fun)(nodecl_t))"
    print "{"
    print "   if (nodecl_is_null(n)) return;"
    print "   fun(n);"
    print "}"
    print "static inline void nodecl_check_list_rule(nodecl_t n, void (*fun)(nodecl_t))"
    print "{"
    print "   ERROR_CONDITION(!nodecl_is_list(n), \"Node must be a list\", 0);"
    print "   int num_items = 0;"
    print "   nodecl_t* list = nodecl_unpack_list(n, &num_items);"
    print "   int i;"
    print "   for (i = 0; i < num_items; i++)"
    print "   {"
    print "      fun(list[i]);"
    print "   }"
    print "   DELETE(list);"
    print "}"
    print "static inline void nodecl_check_nullable_list_rule(nodecl_t n, void (*fun)(nodecl_t))"
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

def from_underscore_to_camel_case_namespaces(x):
    result = ''
    namespaces = []
    previous_is_underscore = True
    for c in x:
        if c == '_' or c == '-':
            previous_is_underscore = True
        elif c == '*':
            previous_is_underscore = True
            namespaces.append(result)
            result = ''
        elif previous_is_underscore:
            previous_is_underscore = False
            result += c.upper()
        else:
            result += c.lower()
    return (tuple(namespaces), result)

def get_all_class_names(rule_map):
    classes = set([])
    for rule_name in rule_map:
        rule_rhs = rule_map[rule_name]
        for rhs in rule_rhs:
            if rhs.__class__ == NodeclStructure:
                classes.add(from_underscore_to_camel_case(rhs.base_name()))
    return classes

def get_all_class_names_and_namespaces(rule_map):
    classes = set([])
    for rule_name in rule_map:
        rule_rhs = rule_map[rule_name]
        for rhs in rule_rhs:
            if rhs.__class__ == NodeclStructure:
                classes.add(from_underscore_to_camel_case_namespaces(rhs.base_name()))
    return classes

def get_all_class_names_and_children_names(rule_map):
    result = []
    classes_set = set([])
    for rule_name in rule_map:
        rule_rhs = rule_map[rule_name]
        for rhs in rule_rhs:
            if rhs.__class__ == NodeclStructure:
                class_name = from_underscore_to_camel_case(rhs.base_name())
                if class_name not in classes_set:
                    classes_set.add(class_name)
                    subtrees = map(lambda x : x[0], rhs.subtrees)
                    result.append((class_name, subtrees, rhs.name_to_underscore(), rhs))
    return result

def get_all_class_names_and_children_names_namespaces(rule_map):
    result = []
    classes_set = set([])
    for rule_name in rule_map:
        rule_rhs = rule_map[rule_name]
        for rhs in rule_rhs:
            if rhs.__class__ == NodeclStructure:
                class_name = from_underscore_to_camel_case_namespaces(rhs.base_name())
                if class_name not in classes_set:
                    classes_set.add(class_name)
                    subtrees = map(lambda x : x[0], rhs.subtrees)
                    result.append((class_name, subtrees, rhs.name_to_underscore(), rhs))
    return result

def get_all_class_names_and_children_names_namespaces_and_modules(rule_map):
    result = []
    classes_set = set([])
    for rule_name in rule_map:
        rule_rhs = rule_map[rule_name]
        for rhs in rule_rhs:
            if rhs.__class__ == NodeclStructure:
                class_name = from_underscore_to_camel_case_namespaces(rhs.base_name())
                module_name = rhs.module_name
                if class_name not in classes_set:
                    classes_set.add(class_name)
                    subtrees = map(lambda x : x[0], rhs.subtrees)
                    result.append((class_name, subtrees, rhs.name_to_underscore(), rhs, module_name))
    return result

def generate_nodecl_classes_fwd_decls(rule_map):
    print "/* Autogenerated file. DO NOT MODIFY. */"
    print "/* Changes in nodecl-generator.py or cxx-nodecl.def will overwrite this file */"
    print "#ifndef TL_NODECL_FWD_HPP"
    print "#define TL_NODECL_FWD_HPP"
    print ""
    print "#include \"tl-nodecl-base-fwd.hpp\""
    print "#include \"mem.h\""
    print ""
    print "namespace Nodecl {"
    classes = get_all_class_names_and_namespaces(rule_map)
    for (namespaces, class_name) in classes:
        for namespace in namespaces:
            print "namespace %s { " % (namespace)
        print "class %s;" % (class_name)
        for namespace in namespaces:
            print "}"
    print ""
    print "} // Nodecl"
    print "#endif // TL_NODECL_FWD_HPP"

def get_qualified_name(namespaces, name):
    return string.join(list(namespaces) + [name], "::")

def generate_visitor_class_header(rule_map):
    print "/* Autogenerated file. DO NOT MODIFY. */"
    print "/* Changes in nodecl-generator.py or cxx-nodecl.def will overwrite this file */"
    print "#ifndef TL_NODECL_VISITOR_HPP"
    print "#define TL_NODECL_VISITOR_HPP"
    print ""
    print "#include <tl-objectlist.hpp>"
    print "#include <tl-nodecl.hpp>"
    print "#include \"cxx-utils.h\""
    print "#include \"mem.h\""
    print ""
    print "namespace Nodecl {"
    print ""
    classes = get_all_class_names_and_namespaces(rule_map)
    print "template <typename _Ret>"
    print "class BaseNodeclVisitor;"
    print "template <>"
    print "class BaseNodeclVisitor<void>;"
    print ""
    print "template <typename _Ret>"
    print "class BaseNodeclVisitor"
    print "{"
    print "   public:"
    print "     typedef _Ret Ret;"
    print "     Ret walk(const NodeclBase&); /* If you override this member function you will be fired */"
    for (namespaces, class_name) in classes:
        qualified_name = get_qualified_name(namespaces, class_name)
        print "     virtual Ret visit(const Nodecl::%s &) = 0;" % (qualified_name)
    print "   virtual _Ret join_list(TL::ObjectList<_Ret> &) = 0;"
    print "   virtual ~BaseNodeclVisitor() { }"
    print "};"
    print "template <>"
    print "class BaseNodeclVisitor<void>"
    print "{"
    print "   public:"
    print "     typedef void Ret;"
    print "     Ret walk(const NodeclBase&); /* If you override this member function you will be fired */"
    for (namespaces, class_name) in classes:
        qualified_name = get_qualified_name(namespaces, class_name)
        print "     virtual Ret visit(const Nodecl::%s &) = 0;" % (qualified_name)
    print "   virtual ~BaseNodeclVisitor() { }"
    print "};"
    print "template <typename _Ret>"
    print "class UnhandledNodeclVisitor : public BaseNodeclVisitor<_Ret>"
    print "{"
    print "   public:"
    print "     typedef typename BaseNodeclVisitor<_Ret>::Ret Ret;"
    print "   virtual Ret unhandled_node(const Nodecl::NodeclBase &) { return Ret(); }"
    for (namespaces, class_name) in classes:
        qualified_name = get_qualified_name(namespaces, class_name)
        print "     virtual Ret visit(const Nodecl::%s & n) { return this->unhandled_node(n); }" % (qualified_name)
    print "   virtual ~UnhandledNodeclVisitor() { }"
    print "};"
    print "template <typename _Ret>"
    print "class NodeclVisitor : public UnhandledNodeclVisitor<_Ret>"
    print "{"
    print "   public:"
    print "     typedef typename UnhandledNodeclVisitor<_Ret>::Ret Ret;"
    print "};"
    print "template <typename _Ret>"
    print "class NodeclVisitor<TL::ObjectList<_Ret> > : public UnhandledNodeclVisitor<TL::ObjectList<_Ret> >"
    print "{"
    print "   public:"
    print "     typedef typename UnhandledNodeclVisitor<TL::ObjectList<_Ret> >::Ret Ret;"
    print "     TL::ObjectList<_Ret> join_list(TL::ObjectList<TL::ObjectList<_Ret> > &list)"
    print "     {"
    print          "TL::ObjectList<_Ret> result;"
    print          "for (typename TL::ObjectList<TL::ObjectList<_Ret> >::iterator it = list.begin(); it != list.end(); it++)"
    print          "{"
    print               "TL::ObjectList<_Ret> &o_list(*it);"
    print               "result.append(o_list);"
    print          "}"
    print          "return result;"
    print "     }"
    print "};"
    print "template <>"
    print "class NodeclVisitor<Nodecl::NodeclBase> : public UnhandledNodeclVisitor<Nodecl::NodeclBase>"
    print "{"
    print "   public:"
    print "     // Only GCC 4.6 can compile this typedef"
    print "     // typedef typename UnhandledNodeclVisitor<Nodecl::NodeclBase>::Ret Ret;"
    print "     virtual Nodecl::NodeclBase join_list(TL::ObjectList<Nodecl::NodeclBase> &list)"
    print "     {"
    print "         return Nodecl::List::make(list);"
    print "     }"
    print "};"

    print "template <typename _Ret>"
    print "class ExhaustiveVisitor : public NodeclVisitor<_Ret>"
    print "{"
    print "public:"
    print "     typedef typename NodeclVisitor<_Ret>::Ret Ret;"
    classes_and_children = get_all_class_names_and_children_names_namespaces_and_modules(rule_map)
    for ((namespaces, class_name), children_name, tree_kind, nodecl_class, module_name) in classes_and_children:
         qualified_name = get_qualified_name(namespaces, class_name)
         print "     virtual Ret visit_pre(const Nodecl::%s & n) { return Ret(); }" % (qualified_name)
         print "     virtual Ret visit_post(const Nodecl::%s & n) { return Ret(); }" % (qualified_name)
         print "     virtual Ret visit(const Nodecl::%s & n)" % (qualified_name)
         print "     {"
         print "        TL::ObjectList<Ret> values;"
         print "        values.append(this->visit_pre(n));"
         child_num = 0
         for child_name in children_name:
              print "        values.append(this->walk(n.get_%s()));" % (child_name)
              child_num = child_num + 1
         print "        values.append(this->visit_post(n));"
         print "        return this->join_list(values);"
         print "     }"
    print "};"
    # ExhaustiveVisitor<void>
    print "template <>"
    print "class ExhaustiveVisitor<void> : public NodeclVisitor<void>"
    print "{"
    print "public:"
    print "     typedef NodeclVisitor<void>::Ret Ret;"
    classes_and_children = get_all_class_names_and_children_names_namespaces_and_modules(rule_map)
    for ((namespaces, class_name), children_name, tree_kind, nodecl_class, module_name) in classes_and_children:
         qualified_name = get_qualified_name(namespaces, class_name)
         print "     virtual Ret visit_pre(const Nodecl::%s & n) { }" % (qualified_name)
         print "     virtual Ret visit_post(const Nodecl::%s & n) { }" % (qualified_name)
         print "     virtual Ret visit(const Nodecl::%s & n)" % (qualified_name)
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
        case AST_NODE_LIST: { TL::ObjectList<Ret> result; AST tree = nodecl_get_ast(n._n); AST it; for_each_element(tree, it) { AST elem = ASTSon1(it);
NodeclBase nb(::_nodecl_wrap(elem)); result.append(this->walk(nb)); } return this->join_list(result); break; }
"""
    node_kind = set([])
    for rule_name in rule_map:
        rule_rhs = rule_map[rule_name]
        for rhs in rule_rhs:
            if rhs.__class__ == NodeclStructure:
                node_kind.add((rhs.name_to_underscore(), from_underscore_to_camel_case_namespaces(rhs.base_name().lower())))
    for (kind_name, (namespaces, class_name)) in node_kind:
        print "       case %s: { return this->visit(static_cast<const Nodecl::%s &>(n)); break; }" % (kind_name, get_qualified_name(namespaces, class_name))
    print """
       default:
           { internal_error("Unexpected tree kind '%s'\\n", ast_print_node_type(n.get_kind())); }
    }

    return Ret();
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
    print "#include \"mem.h\""
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
                node_kind.add((rhs.name_to_underscore(), from_underscore_to_camel_case_namespaces(rhs.base_name().lower())))
    for (kind_name, (namespaces, class_name)) in node_kind:
        print "       case %s: { this->visit(static_cast<const Nodecl::%s &>(n)); break; }" % (kind_name, get_qualified_name(namespaces, class_name))
    print """
       default:
           { internal_error("Unexpected tree kind '%s'\\n", ast_print_node_type(n.get_kind())); }
    }
"""
    print "}"
    print "} /* namespace Nodecl */"
    print "#endif // TL_NODECL_VISITOR_CPP"

def generate_copy_visitor_class_header(rule_map):
    print "/* Autogenerated file. DO NOT MODIFY. */"
    print "/* Changes in nodecl-generator.py or cxx-nodecl.def will overwrite this file */"
    print "#ifndef TL_NODECL_COPY_VISITOR_HPP"
    print "#define TL_NODECL_COPY_VISITOR_HPP"
    print ""
    print "#include \"tl-nodecl-visitor.hpp\""
    print "#include \"tl-nodecl-copy-visitor-base.hpp\""
    print "#include \"mem.h\""
    print ""
    print "namespace Nodecl {"
    print "   class ShallowCopyVisitor : public CopyVisitorBase"
    print "   {"
    print "      public:"
    classes_and_children = get_all_class_names_and_children_names_namespaces(rule_map)
    for ((namespaces, class_name), children_name, tree_kind, nodecl_class) in classes_and_children:
         qualified_name = get_qualified_name(namespaces, class_name)
         print "        virtual Ret visit(const Nodecl::%s & n);" % (qualified_name)
    print "   };"
    print "   struct SymbolMap"
    print "   {"
    print "       virtual TL::Symbol map(TL::Symbol sym) = 0;"
    print "       virtual void add_map(TL::Symbol source, TL::Symbol target) = 0;"
    print "   };"
    print "   class DeepCopyVisitorBase : public CopyVisitorBase"
    print "   {"
    print "      protected:"
    print "         SymbolMap &_map_symbol;"
    print "      public:"
    print "          DeepCopyVisitorBase(SymbolMap &map_symbol) : _map_symbol(map_symbol) { }"
    classes_and_children = get_all_class_names_and_children_names_namespaces(rule_map)
    for ((namespaces, class_name), children_name, tree_kind, nodecl_class) in classes_and_children:
         qualified_name = get_qualified_name(namespaces, class_name)
         print "        virtual Ret visit(const Nodecl::%s & n);" % (qualified_name)
    print "   };"
    print "}"
    print "#endif // TL_NODECL_COPY_VISITOR_HPP"

def generate_copy_visitor_class_impl(rule_map):
    print "/* Autogenerated file. DO NOT MODIFY. */"
    print "/* Changes in nodecl-generator.py or cxx-nodecl.def will overwrite this file */"
    print "#include \"tl-nodecl-copy-visitor.hpp\""
    print ""
    print "namespace Nodecl {"
    classes_and_children = get_all_class_names_and_children_names_namespaces(rule_map)
    for ((namespaces, class_name), children_name, tree_kind, nodecl_class) in classes_and_children:
         qualified_name = get_qualified_name(namespaces, class_name)
         print "  ShallowCopyVisitor::Ret ShallowCopyVisitor::visit(const Nodecl::%s & n)" % (qualified_name)
         print "  {"

         factory_arguments = []
         for child_name in children_name:
             print "Nodecl::NodeclBase child_%s = walk(n.get_%s());" % (child_name, child_name)
             factory_arguments.append("child_%s" % (child_name))

         if nodecl_class.needs_symbol:
             print "TL::Symbol symbol = n.get_symbol();"
             factory_arguments.append("symbol")
         if nodecl_class.needs_type:
             print "TL::Type type = n.get_type();";
             factory_arguments.append("type")
         if nodecl_class.needs_text:
             print "const std::string& text = n.get_text();"
             factory_arguments.append("text")
         if nodecl_class.needs_cval:
             print "const_value_t* cval = n.get_constant();"
             factory_arguments.append("cval")
         if nodecl_class.needs_template_parameters:
             print "template_parameter_list_t* template_parameters = nodecl_get_template_parameters(n.get_internal_nodecl());"
             factory_arguments.append("template_parameters")
         if nodecl_class.needs_decl_context:
             print "TL::Scope sc = nodecl_get_decl_context(n.get_internal_nodecl());"
             factory_arguments.append("sc")

         print "const locust_t* &location = nodecl_get_locus(n.get_internal_nodecl());"
         factory_arguments.append("location")

         print "return %s::make(%s);" % (qualified_name, string.join(factory_arguments, ", "))

         print "  }"
    for ((namespaces, class_name), children_name, tree_kind, nodecl_class) in classes_and_children:
         qualified_name = get_qualified_name(namespaces, class_name)
         print "  DeepCopyVisitorBase::Ret DeepCopyVisitorBase::visit(const Nodecl::%s & n)" % (qualified_name)
         print "  {"

         factory_arguments = []
         for child_name in children_name:
             print "Nodecl::NodeclBase child_%s = walk(n.get_%s());" % (child_name, child_name)
             factory_arguments.append("child_%s" % (child_name))

         if nodecl_class.needs_symbol:
             print "TL::Symbol symbol = _map_symbol.map( n.get_symbol() );"
             factory_arguments.append("symbol")
         if nodecl_class.needs_type:
             print "TL::Type type = n.get_type();";
             factory_arguments.append("type")
         if nodecl_class.needs_text:
             print "const std::string& text = n.get_text();"
             factory_arguments.append("text")
         if nodecl_class.needs_cval:
             print "const_value_t* cval = n.get_constant();"
             factory_arguments.append("cval")
         if nodecl_class.needs_template_parameters:
             print "template_parameter_list_t* template_parameters = nodecl_get_template_parameters(n.get_internal_nodecl());"
             factory_arguments.append("template_parameters")
         if nodecl_class.needs_decl_context:
             print "TL::Scope sc = nodecl_get_decl_context(n.get_internal_nodecl());";
             factory_arguments.append("sc")

         print "const locust_t* &location = nodecl_get_locus(n.get_internal_nodecl());"
         factory_arguments.append("location")

         print "return %s::make(%s);" % (qualified_name, string.join(factory_arguments, ", "))

         print "  }"
    print "}"

def generate_nodecl_classes_base(rule_map):
   print "/* Autogenerated file. DO NOT MODIFY. */"
   print "/* Changes in nodecl-generator.py or cxx-nodecl.def will overwrite this file */"
   print "#ifndef TL_NODECL_HPP"
   print "#define TL_NODECL_HPP"
   print ""
   print "#include <string>"
   print "#include \"tl-nodecl-base.hpp\""
   print "#include \"mem.h\""
   
   print "namespace Nodecl {"

   classes_and_children = get_all_class_names_and_children_names_namespaces(rule_map)
   for ((namespaces, class_name), children_name, tree_kind, nodecl_class) in classes_and_children:
       qualified_name = get_qualified_name(namespaces, class_name)
       print "class %s : public NodeclBase" % (qualified_name)
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

       factory_parameters.append("const locus_t *location = ::make_locus(\"\", 0, 0)");

       print "    // Factory method"
       print "    static %s make(%s);" % (class_name, string.join(factory_parameters, ", "))
       print ""

       if children_name:
            print "    // Children getters and setters "
       child_num = 0
       for child_name in children_name:
            print "    NodeclBase get_%s() const { return NodeclBase(nodecl_get_child(_n, %d)); } " % (child_name, child_num)
            print "    void set_%s(const Nodecl::NodeclBase &n) { nodecl_set_child(_n, %d, n.get_internal_nodecl()); } " % (child_name, child_num)
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
   print "#include \"mem.h\""
   print ""
   print "namespace Nodecl {"
   classes_and_children = get_all_class_names_and_children_names_namespaces(rule_map)
   for ((namespaces, class_name), children_name, tree_kind, nodecl_class) in classes_and_children:

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
           factory_arguments.append("::uniquestr(text.c_str())");
       if nodecl_class.needs_cval:
           factory_parameters.append("const_value_t* cval");
           factory_arguments.append("cval");
       if nodecl_class.needs_template_parameters:
           factory_parameters.append("template_parameter_list_t* template_parameters");
           factory_arguments.append("template_parameters");
       if nodecl_class.needs_decl_context:
           factory_parameters.append("TL::Scope scope");
           factory_arguments.append("scope.get_decl_context()");

       factory_parameters.append("const locus_t* location")
       factory_arguments.append("location");

       nodecl_make_name = "nodecl_make_%s" % (nodecl_class.base_name_to_underscore_lowercase())

       qualified_name = get_qualified_name(namespaces, class_name)
       print "%s %s::make(%s)" % (qualified_name, qualified_name, string.join(factory_parameters, ", "))
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
   print "#include \"cxx-type-fwd.h\""
   print "#include \"cxx-cexpr-fwd.h\""
   print "#include \"cxx-scope-decls.h\""
   print "#include \"mem.h\""
   print ""
   print "MCXX_BEGIN_DECLS"
   print ""

   for i in range(1, 7):
        params = map(lambda x : "nodecl_t element%d" % (x) , range(0, i))
        print "nodecl_t nodecl_make_list_%d(%s);" % (i, string.join(params, ", "))
   print "nodecl_t nodecl_make_list_n(int num_items, nodecl_t* items);"
   print ""
   classes = {}
   for rule_name in rule_map:
       rule_rhs = rule_map[rule_name]
       for rhs in rule_rhs:
           if rhs.__class__ == NodeclStructure:
               classes[rhs.base_name_to_underscore_lowercase()] = rhs
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
           param_list_nodecl.append("const decl_context_t*");
       param_list_nodecl.append("const locus_t* location");

       print "nodecl_t nodecl_make_%s(%s);" % (key, string.join(param_list_nodecl, ", "))
   print ""
   print "MCXX_END_DECLS"
   print ""
   print "#endif // CXX_NODECL_OUTPUT_H"
# print "key %s -> value %s" % (repr(key), repr(value))


def generate_routines_impl(rule_map):
   print "/* Autogenerated file. DO NOT MODIFY. */"
   print "/* Changes in nodecl-generator.py or cxx-nodecl.def will overwrite this file */"
   print "#include <stdlib.h>"
   print "#include \"cxx-nodecl-output.h\""
   print "#include \"cxx-exprtype.h\""
   print "#include \"cxx-utils.h\""
   print "#include \"mem.h\""
   print ""
   for i in range(2, 7):
        params = map(lambda x : "nodecl_t element%d" % (x) , range(0, i))
        args = map(lambda x : "element%d" % (x) , range(0, i))
        print "nodecl_t nodecl_make_list_%d(%s)" % (i, string.join(params, ", "))
        print "{"
        print "  return nodecl_append_to_list(nodecl_make_list_%d(%s), %s);" % (i-1, string.join(args[:-1], ", "), args[-1])
        print "}"
   print "nodecl_t nodecl_make_list_n(int num_items, nodecl_t* items)"
   print "{"
   print "   int i;"
   print "   nodecl_t result = nodecl_null();"
   print "   for (i = 0; i < num_items; i++) result = nodecl_append_to_list(result, items[i]);"
   print "   return result;"
   print "}"

   classes = {}
   for rule_name in rule_map:
       rule_rhs = rule_map[rule_name]
       for rhs in rule_rhs:
           if rhs.__class__ == NodeclStructure:
               classes[rhs.base_name_to_underscore_lowercase()] = rhs
   for (key, rhs_rule) in classes.iteritems() :
       param_list_nodecl = []
       param_name_list = []
       for item in rhs_rule.subtrees:
           i = 0
           pattern = item[1].replace("-", "_").replace("*", "_") + "_%d"
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
           param_list_nodecl.append("const decl_context_t* decl_context");
       param_list_nodecl.append("const locus_t* location");
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
              print "  internal_error(\"Null node not allowed in node %d nodecl_make_%s. Location: %%s\\n\", locus_to_str(location));" % (i, key)
              print "}"
          if subrule_ref.is_nullable():
             print "if (!nodecl_is_null(checked_tree))"
             print "{"
          if subrule_ref.is_seq():
             print " if (!nodecl_is_list(checked_tree))"
             print " {"
             print "  internal_error(\"Node must be a list in node %d of nodecl_make_%s. Location: %%s\\n\", locus_to_str(location));" % (i, key)
             print " }"
             print "AST list = nodecl_get_ast(checked_tree), it;"
             print "for_each_element(list, it)"
             print "{"
             print "  checked_tree = _nodecl_wrap(ASTSon1(it));"
          if first_set :
              checks = map(lambda x : "(nodecl_get_kind(checked_tree) != %s)" % (x), first_set)
              print "if (%s)" % (string.join(checks, "\n&& "))
              print "{"
              print "  internal_error(\"Invalid node %d of type %%s in nodecl_make_%s. Location: %%s\\n\", ast_print_node_type(nodecl_get_kind(checked_tree)), locus_to_str(location));" % (i, key)
              print "}"
          if subrule_ref.is_seq():
             print "}"
          if subrule_ref.is_nullable():
             print "}"
          i = i + 1
          print "}"

       if rhs_rule.needs_text:
           print "  if (text == NULL) internal_error(\"This node requires a text. Location: %s\", locus_to_str(location));"
           text_value = "text";
       else:
           text_value = "NULL"

       # Build the node
       print "  nodecl_t result = nodecl_null();"
       num_children = len(rhs_rule.subtrees)
       if num_children == 0:
          print "  result.tree = ASTLeaf(%s, location, %s);" % (rhs_rule.name_to_underscore(), text_value)
       else:
          print "  result.tree = ASTMake%d(%s, %s, location, %s);" % (num_children, rhs_rule.name_to_underscore(), \
                 string.join(map(lambda x : x + ".tree", param_name_list), ", "), text_value);

       if rhs_rule.needs_symbol:
           print "  if (symbol == NULL) internal_error(\"Node requires a symbol. Location: %s\", locus_to_str(location));"
           print "  nodecl_set_symbol(result, symbol);"
       if rhs_rule.needs_type:
           print "  if (type == NULL) internal_error(\"This node requires a type. Location: %s\", locus_to_str(location));"
           print "  nodecl_set_type(result, type);"
       if rhs_rule.needs_cval:
           print "  if (cval == NULL) internal_error(\"This node requires a constant value. Location: %s\", locus_to_str(location));"
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
                node_kind.add(rhs.base_name_to_underscore_lowercase())
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
#include "mem.h"

void nodecl_walk(nodecl_external_visitor_t* external_visitor, nodecl_t n)
{
    AST tree = nodecl_get_ast(n);
    if (tree == NULL)
        return;
    switch (ASTKind(tree))
    {
        case AST_NODE_LIST: { AST it; for_each_element(tree, it) { AST elem = ASTSon1(it); nodecl_walk(external_visitor, _nodecl_wrap(elem)); } break; }
"""
    node_kind = set([])
    for rule_name in rule_map:
        rule_rhs = rule_map[rule_name]
        for rhs in rule_rhs:
            if rhs.__class__ == NodeclStructure:
                node_kind.add((rhs.name_to_underscore(), rhs.base_name_to_underscore().lower()))
    for node in node_kind:
        print "       case %s: { if (external_visitor->visit_%s != NULL) external_visitor->visit_%s(external_visitor, n); break; }" % (node[0], node[1], node[1])
    print """
       default:
           { internal_error("Unexpected tree kind '%s'\\n", ast_print_node_type(ASTKind(tree))); }
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
                node_kind.add(rhs.name_to_underscore())
    l = list(node_kind)
    l.sort()
    for kind_name in l:
        print kind_name

def generate_c_shallow_copy_def(rule_map):
    print "#include \"cxx-nodecl.h\""
    print "#include \"cxx-nodecl-output.h\""
    print "#include \"cxx-scope.h\""
    print "#include \"cxx-utils.h\""
    print "#include \"mem.h\""
    print "/* Autogenerated file. DO NOT MODIFY. */"
    print "/* Changes in nodecl-generator.py or cxx-nodecl.def will overwrite this file */"

    print """
nodecl_t nodecl_shallow_copy(nodecl_t n)
{
    if (nodecl_is_null(n))
        return nodecl_null();
    switch (nodecl_get_kind(n))
    {
        case AST_NODE_LIST:
        {
          int num_items = 0;
          nodecl_t result = nodecl_null();
          nodecl_t* list = nodecl_unpack_list(n, &num_items);
          int i;
          for (i = 0; i < num_items; i++)
          {
                  result = nodecl_append_to_list(result, nodecl_shallow_copy(list[i]));
          }
          DELETE(list);
          return result;
          break;
        }
"""

    node_kind = []
    for rule_name in rule_map:
        rule_rhs = rule_map[rule_name]
        for rhs in rule_rhs:
            if rhs.__class__ == NodeclStructure:
                node_kind.append((rhs.name_to_underscore(), rhs.base_name_to_underscore().lower(), rhs))
    for node in node_kind:
        nodecl_class = node[2]
        print "       case %s:" % (node[0])
        print "       {"
        factory_arguments = []
        i = 0
        for subtree in nodecl_class.subtrees:
            (rule_label, rule_ref) = subtree
            current_rule = RuleRef(rule_ref)

            if current_rule.canonical_rule() != "any":
                print "nodecl_t child_%d = nodecl_shallow_copy(nodecl_get_child(n, %d));" % (i, i)
            else:
                print "nodecl_t child_%d = _nodecl_wrap(ast_copy(nodecl_get_ast(nodecl_get_child(n, %d))));" % (i, i)

            factory_arguments.append("child_%d" % (i))
            i = i + 1

        needs_attr = lambda x : getattr(nodecl_class, "needs_%s" % (x))
        may_have_attr = lambda x : getattr(nodecl_class, "may_have_%s" % (x))
        has_attr = lambda x : needs_attr(x) or may_have_attr(x)

        if has_attr("symbol"):
            print "scope_entry_t* symbol = nodecl_get_symbol(n);"
        if needs_attr("symbol"):
            factory_arguments.append("symbol")

        if has_attr("type"):
            print "type_t* type = nodecl_get_type(n);"
        if needs_attr("type"):
            factory_arguments.append("type")

        if has_attr("text"):
            print "const char* text = nodecl_get_text(n);"
        if needs_attr("text"):
            factory_arguments.append("text")

        if has_attr("cval"):
            print "const_value_t* cval = nodecl_get_constant(n);"
        if needs_attr("cval"):
            factory_arguments.append("cval")

        if has_attr("template_parameters"):
            print "template_parameter_list_t* template_parameters = nodecl_get_template_parameters(n);"
        if needs_attr("template_parameters"):
            factory_arguments.append("template_parameters")

        if has_attr("decl_context"):
            print "const decl_context_t* decl_context = nodecl_get_decl_context(n);";
        if needs_attr("decl_context"):
            factory_arguments.append("decl_context")

        print "const locus_t* location = nodecl_get_locus(n);"
        factory_arguments.append("location")
        print "nodecl_t result = nodecl_make_%s(%s);" % (node[1], string.join(factory_arguments, ", "))

        if may_have_attr("symbol"):
            print "nodecl_set_symbol(result, symbol);"

        if may_have_attr("type"):
            print "nodecl_set_type(result, type);"

        if may_have_attr("text"):
            print "nodecl_set_text(result, text);"

        if may_have_attr("cval"):
            print "nodecl_set_constant(result, cval);"

        if may_have_attr("template_parameters"):
            print "nodecl_set_template_parameters(result, template_parameters);"

        if may_have_attr("decl_context"):
            print "nodecl_set_decl_context(result, decl_context);"

        # Extra attributes from expressions
        print "nodecl_expr_set_is_value_dependent(result, nodecl_expr_is_value_dependent(n));"
        print "nodecl_expr_set_is_type_dependent(result, nodecl_expr_is_type_dependent(n));"

        print "       return result;";
        print "       break;"
        print "       }"
    print """
       default:
           { internal_error("Unexpected tree kind '%s'\\n", ast_print_node_type(nodecl_get_kind(n))); }
       }
       return nodecl_null();
}
"""

def generate_c_deep_copy_def(rule_map):
    print "#include \"cxx-nodecl.h\""
    print "#include \"cxx-nodecl-deep-copy.h\""
    print "#include \"cxx-symbol-deep-copy.h\""
    print "#include \"cxx-cexpr-deep-copy.h\""
    print "#include \"cxx-nodecl-output.h\""
    print "#include \"cxx-scope.h\""
    print "#include \"cxx-typeutils.h\""
    print "#include \"cxx-utils.h\""
    print "#include \"mem.h\""
    print "/* Autogenerated file. DO NOT MODIFY. */"
    print "/* Changes in nodecl-generator.py or cxx-nodecl.def will overwrite this file */"

    print """
extern nodecl_t nodecl_deep_copy_context(nodecl_t n, const decl_context_t* new_decl_context, 
   symbol_map_t* symbol_map,
   symbol_map_t** synth_symbol_map,
   nodecl_deep_copy_map_t* nodecl_deep_copy_map,
   symbol_deep_copy_map_t* symbol_deep_copy_map);
extern nodecl_t nodecl_deep_copy_function_code(nodecl_t n, const decl_context_t* new_decl_context, 
   symbol_map_t* symbol_map,
   symbol_map_t** synth_symbol_map,
   nodecl_deep_copy_map_t* nodecl_deep_copy_map,
   symbol_deep_copy_map_t* symbol_deep_copy_map);

extern void nodecl_deep_copy_map_add(
   nodecl_deep_copy_map_t* nodecl_deep_copy_map,
   nodecl_t orig,
   nodecl_t copied);

nodecl_t nodecl_deep_copy_rec(nodecl_t n, const decl_context_t* new_decl_context,
   symbol_map_t* symbol_map,
   symbol_map_t** synth_symbol_map,
   nodecl_deep_copy_map_t* nodecl_deep_copy_map,
   symbol_deep_copy_map_t* symbol_deep_copy_map)
{
    *synth_symbol_map = symbol_map;

    if (nodecl_is_null(n))
        return nodecl_null();
    nodecl_t result;
    switch (nodecl_get_kind(n))
    {
        case AST_NODE_LIST:
        {
          int num_items = 0;
          result = nodecl_null();
          nodecl_t* list = nodecl_unpack_list(n, &num_items);
          int i;
          for (i = 0; i < num_items; i++)
          {
                  result = nodecl_append_to_list(result, nodecl_deep_copy_rec(list[i], new_decl_context,
                          *synth_symbol_map, synth_symbol_map, nodecl_deep_copy_map, symbol_deep_copy_map));
          }
          DELETE(list);
          break;
        }
"""
    node_kind = []
    for rule_name in rule_map:
        rule_rhs = rule_map[rule_name]
        for rhs in rule_rhs:
            if rhs.__class__ == NodeclStructure:
                node_kind.append((rhs.name_to_underscore(), rhs.base_name_to_underscore().lower(), rhs))
    for node in node_kind:
        nodecl_class = node[2]
        print "       case %s:" % (node[0])
        print "       {"

        if node[0] == "NODECL_CONTEXT":
            print "          result = nodecl_deep_copy_context(n, new_decl_context, (*synth_symbol_map), synth_symbol_map, nodecl_deep_copy_map, symbol_deep_copy_map);"
            print "          nodecl_deep_copy_map_add(nodecl_deep_copy_map, n, result);"
            print "          return result;"
            print "       }"
            continue
        elif node[0] == "NODECL_FUNCTION_CODE":
            print "          result = nodecl_deep_copy_function_code(n, new_decl_context, (*synth_symbol_map), synth_symbol_map, nodecl_deep_copy_map, symbol_deep_copy_map);"
            print "          nodecl_deep_copy_map_add(nodecl_deep_copy_map, n, result);"
            print "          return result;"
            print "       }"
            continue

        factory_arguments = []
        i = 0
        for subtree in nodecl_class.subtrees:
            (rule_label, rule_ref) = subtree
            current_rule = RuleRef(rule_ref)

            if current_rule.canonical_rule() != "any":
                print "nodecl_t child_%d = nodecl_deep_copy_rec(nodecl_get_child(n, %d), new_decl_context, (*synth_symbol_map), synth_symbol_map, nodecl_deep_copy_map, symbol_deep_copy_map);" % (i, i)
            else:
                print "nodecl_t child_%d = _nodecl_wrap(ast_copy(nodecl_get_ast(nodecl_get_child(n, %d))));" % (i, i);

            factory_arguments.append("child_%d" % (i))
            i = i + 1

        needs_attr = lambda x : getattr(nodecl_class, "needs_%s" % (x))
        may_have_attr = lambda x : getattr(nodecl_class, "may_have_%s" % (x))
        has_attr = lambda x : needs_attr(x) or may_have_attr(x)

        if has_attr("symbol"):
            print "scope_entry_t* symbol = (*synth_symbol_map)->map(*synth_symbol_map, nodecl_get_symbol(n));"
        if needs_attr("symbol"):
            factory_arguments.append("symbol")

        # FIXME - The type may have to be regenerated as well
        if has_attr("type"):
            print "type_t* type = nodecl_get_type(n);"
            print "type = type_deep_copy_compute_maps(type, /* dest */ NULL, new_decl_context, symbol_map, nodecl_deep_copy_map, symbol_deep_copy_map);"
        if needs_attr("type"):
            factory_arguments.append("type")

        if has_attr("text"):
            print "const char* text = nodecl_get_text(n);"
        if needs_attr("text"):
            factory_arguments.append("text")

        if has_attr("cval"):
            print "const_value_t* cval = const_value_deep_copy(nodecl_get_constant(n), *synth_symbol_map);"
        if needs_attr("cval"):
            factory_arguments.append("cval")

        if has_attr("template_parameters"):
            print "template_parameter_list_t* template_parameters = nodecl_get_template_parameters(n);"
        if needs_attr("template_parameters"):
            factory_arguments.append("template_parameters")

        if has_attr("decl_context"):
            print "const decl_context_t* decl_context = nodecl_get_decl_context(n);";
        if needs_attr("decl_context"):
            factory_arguments.append("decl_context")

        print "const locus_t* location = nodecl_get_locus(n);"
        factory_arguments.append("location")

        print "result = nodecl_make_%s(%s);" % (node[1], string.join(factory_arguments, ", "))

        if may_have_attr("symbol"):
            print "nodecl_set_symbol(result, symbol);"

        if may_have_attr("type"):
            print "nodecl_set_type(result, type);"

        if may_have_attr("text"):
            print "nodecl_set_text(result, text);"

        if may_have_attr("cval"):
            print "nodecl_set_constant(result, cval);"

        if may_have_attr("template_parameters"):
            print "nodecl_set_template_parameters(result, template_parameters);"

        if may_have_attr("decl_context"):
            print "nodecl_set_decl_context(n, decl_context);"

        print "       break;"
        print "       }"
    print """
       default:
           { internal_error("Unexpected tree kind '%s'\\n", ast_print_node_type(nodecl_get_kind(n))); }
       }

       nodecl_deep_copy_map_add(nodecl_deep_copy_map, n, result);

       return result;
}
"""


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
elif op_mode == "c_visitor_decl":
    generate_c_visitor_decl(rule_map)
elif op_mode == "c_visitor_def":
    generate_c_visitor_def(rule_map)
elif op_mode == "asttype_nodecl":
    generate_asttypes(rule_map)
elif op_mode == "c_shallow_copy_def":
    generate_c_shallow_copy_def(rule_map)
elif op_mode == "c_deep_copy_def":
    generate_c_deep_copy_def(rule_map)
elif op_mode == "cxx_nodecl_class_fwd_header":
    generate_nodecl_classes_fwd_decls(rule_map)
elif op_mode == "cxx_nodecl_class_header":
    generate_nodecl_classes_base(rule_map)
elif op_mode == "cxx_nodecl_class_impl":
    generate_nodecl_classes_specs(rule_map)
elif op_mode == "cxx_visitor_decl":
    generate_visitor_class_header(rule_map)
elif op_mode == "cxx_visitor_impl":
    generate_visitor_class_impl(rule_map)
elif op_mode == "cxx_copy_visitor_decl":
    generate_copy_visitor_class_header(rule_map)
elif op_mode == "cxx_copy_visitor_impl":
    generate_copy_visitor_class_impl(rule_map)
else:
    raise Exception("Invalid op_mode %s" % (op_mode))

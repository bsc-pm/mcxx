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

import inspect

def lineno():
    """Returns the current line number in our program."""
    return inspect.currentframe().f_back.f_lineno

f = open(sys.argv[1])

if (len(sys.argv) >= 3):
  op = sys.argv[2]
else:
  op = "entity_specifiers"

def loadlines(f):
    lines = f.readlines()
    result = []
    for l in lines:
        l = l.strip(" \n")
        if l[0] == '#':
            continue
        result.append(l)
    return result


def check_file(lines):
  for l in lines:
      fields = l.split("|");
      if len(fields) != 4:
        raise Exception, "Invalid number of fields"

def get_up_to_matching_paren(s):
    if (s[0] != '('):
        raise Exception, "String must start with ("
    level = 1
    for i in range(1, len(s)):
        c = s[i]
        if (c == '('):
            level = level + 1
        elif (c == ')'):
            level = level-1
            if (level == 0):
                return s[1:i]
        else:
            pass
    raise Exception, "No matching left parent"


class TypeKind:
    BIT = 1
    INTEGER = 2
    POINTER = 3
    OTHER = 5

def print_type_and_name(_type, name):
    _type = _type.strip(" \n")
    if (_type == "bool"):
        return [("bool", name, ":1", TypeKind.BIT)]
    elif (_type == "integer"):
        return [("int", name, "", TypeKind.INTEGER)]
    elif (_type == "AST"):
        return [("AST", name, "", TypeKind.POINTER)]
    elif (_type == "nodecl"):
        return [("nodecl_t", name, "", TypeKind.POINTER)]
    elif (_type == "type"):
        return [("type_t*", name, "", TypeKind.POINTER)]
    elif (_type == "string"):
        return [("const char*", name, "", TypeKind.POINTER)]
    elif (_type == "symbol"):
        return [("scope_entry_t*", name, "", TypeKind.POINTER)]
    elif (_type == "scope"):
        return [("const decl_context_t*", name, "", TypeKind.OTHER)]
    elif (_type.startswith("typeof")):
        parts_of_type_name = get_up_to_matching_paren(_type[len("typeof"):])
        parts_of_type = parts_of_type_name.split(",");
        type_name = parts_of_type[0].strip()
        if (len(parts_of_type) == 1):
            kind = TypeKind.OTHER
        elif (len(parts_of_type) == 2):
            map_kinds = { "pointer" : TypeKind.POINTER,
                          "intptr" : TypeKind.POINTER,
                          "integer" : TypeKind.INTEGER,
                          "enum" : TypeKind.INTEGER }
            kind = map_kinds.get(parts_of_type[1].strip(), TypeKind.OTHER)
        return [(type_name, name, "", kind)]
    elif (_type.startswith("pointer")):
        type_name = get_up_to_matching_paren(_type[len("pointer"):])
        (t, n, s, k) = print_type_and_name(type_name, name)[0]
        return [(t + "*", n, s, TypeKind.POINTER)]
    elif (_type.startswith("array")):
        type_name = get_up_to_matching_paren(_type[len("array"):])
        field_names = name.split(",")
        if (len(field_names) == 1):
           num_name = "num_" + name
           list_name = name
        elif (len(field_names) == 2):
            num_name = field_names[0]
            list_name = field_names[1]
        else:
            raise Exception("Invalid number of fields in array name. Only 1 or 2 comma-separated are allowed")
        return print_type_and_name("integer", num_name) + print_type_and_name("pointer(" + type_name + ")", list_name) 
    else:
        raise Exception("Invalid type %s" % (_type))

def print_entity_specifiers(lines):
    indent = " " * 4
    current_language = "all"
    decls = []
    for l in lines:
      fields = l.split("|");
      (_type,language,name,description) = fields
      if name[0] == "*":
          name = name[1:]

      language = language.strip(" \n")
      if (language != current_language) :
          current_language = language
      descr = description.strip(" \n")
      decls += print_type_and_name(_type, name)

    print """
#ifndef CXX_ENTITY_SPECIFIERS_H
#define CXX_ENTITY_SPECIFIERS_H

#include <stdbool.h>

// Include this file only from cxx-scope-decls.h and not from anywhere else

typedef struct entity_specifiers_tag\n{"""

    for tk in [TypeKind.OTHER, TypeKind.POINTER, TypeKind.INTEGER, TypeKind.BIT]:
       for d in decls:
         (typename, name, suffix, k) = d
         if k == tk:
             print indent + typename + " " + name + suffix + ";"

    print "} entity_specifiers_t;"
    print ""
    print "#endif"

def print_getters_setters(lines):
    print """
#ifndef CXX_ENTITY_SPECIFIERS_OPS_H
#define CXX_ENTITY_SPECIFIERS_OPS_H

#include <stdbool.h>

// We need string-utils.h for P_LIST_ADD and similar macros
#include "string_utils.h"

// We need cxx-process.h for ERROR_CONDITION
#include "cxx-process.h"

// Include this file only from cxx-scope-decls.h and not from anywhere else

"""
    current_language = "all"
    decls = []
    for l in lines:
      fields = l.split("|");
      (_type,language,name,description) = fields
      if name[0] == "*":
          name = name[1:]

      # Atomic types
      if (_type in ["integer", "bool", "string", "AST", "nodecl", "type", "symbol"]) or _type.startswith("typeof"):
         decls = print_type_and_name(_type, name)
         if len(decls) != 1:
             raise Exception("Expecting one declaration")
         (typename, name, suffix, k) = decls[0]
         print "// Single value attribute: '%s' " % (name)
         print "static inline %s symbol_entity_specs_get_%s(scope_entry_t* s)\n{\n    return s->_entity_specs.%s;\n}" % (typename, name, name)
         print "static inline void symbol_entity_specs_set_%s(scope_entry_t* s, %s v)\n{\n    s->_entity_specs.%s = v;\n}" % (name, typename, name)
         print ""
      # Compound types
      elif _type.startswith("array"):
          type_name = get_up_to_matching_paren(_type[len("array"):])
          field_names = name.split(",")
          if (len(field_names) == 1):
             num_name = "num_" + name
             list_name = name
          elif (len(field_names) == 2):
              num_name = field_names[0]
              list_name = field_names[1]
          else:
              raise Exception("Invalid number of fields in array name. Only 1 or 2 comma-separated are allowed")

          decls = print_type_and_name(type_name, "")
          if len(decls) != 1:
              raise Exception("Expecting one declaration")
          (type_name, _name, _suffix, _k) = decls[0]

          # These types cannot be compared in C
          cannot_be_compared = ["function_parameter_info_t", "gcc_attribute_t"]

          print "// Multiple value attribute: '%s' " % (list_name)
          if num_name != "num_" + name:
              print "// Note: The number of values of this attribute is stored in attribute '%s'" % (num_name)
          
          print "static inline int symbol_entity_specs_get_%s(scope_entry_t* s)\n{\n    return s->_entity_specs.%s;\n}" % (num_name, num_name)
          print "static inline %s symbol_entity_specs_get_%s_num(scope_entry_t* s, int i)\n{\n    ERROR_CONDITION(i >= s->_entity_specs.%s,\n        \"Invalid index %%d >= %%d\",\n        i, s->_entity_specs.%s);\n    return s->_entity_specs.%s[i];\n}" % (type_name, list_name, num_name, num_name, list_name)
          print "static inline void symbol_entity_specs_set_%s_num(scope_entry_t* s, int i, %s v)\n{\n    ERROR_CONDITION(i >= s->_entity_specs.%s,\n        \"Invalid index %%d >= %%d\",\n         i, s->_entity_specs.%s);\n    s->_entity_specs.%s[i] = v;\n}" % (list_name, type_name, num_name, num_name, list_name)
          print "static inline void symbol_entity_specs_append_%s(scope_entry_t* s, %s item)\n{\n    P_LIST_ADD(s->_entity_specs.%s, s->_entity_specs.%s, item);\n}" % (list_name, type_name, list_name, num_name)
          if type_name not in cannot_be_compared:
              print "static inline void symbol_entity_specs_remove_%s(scope_entry_t* s, %s item)\n{\n    P_LIST_REMOVE(s->_entity_specs.%s, s->_entity_specs.%s, item);\n}" % (list_name, type_name, list_name, num_name)
              print "static inline void symbol_entity_specs_insert_%s(scope_entry_t* s, %s item)\n{\n    P_LIST_ADD_ONCE(s->_entity_specs.%s, s->_entity_specs.%s, item);\n}" % (list_name, type_name, list_name, num_name)
          print "static inline void symbol_entity_specs_remove_%s_cmp(scope_entry_t* s, %s item,\n        char (*cmp)(%s, %s))\n{\n    P_LIST_REMOVE_FUN(s->_entity_specs.%s, s->_entity_specs.%s, item, cmp);\n}" % (list_name, type_name, type_name, type_name, list_name, num_name)
          print "static inline void symbol_entity_specs_insert_%s_cmp(scope_entry_t* s, %s item,\n        char (*cmp)(%s, %s))\n{\n    P_LIST_ADD_ONCE_FUN(s->_entity_specs.%s, s->_entity_specs.%s, item, cmp);\n}" % (list_name, type_name, type_name, type_name, list_name, num_name)
          print "static inline void symbol_entity_specs_add_%s(scope_entry_t* s, %s item)\n{\n    symbol_entity_specs_append_%s(s, item);\n}" % (list_name, type_name, list_name)
          print "static inline void symbol_entity_specs_reserve_%s(scope_entry_t* s, int num)\n{\n    s->_entity_specs.%s = num;\n    s->_entity_specs.%s = NEW_VEC0(%s, num);\n}" % (list_name, num_name, list_name, type_name)
          # print "static inline void symbol_entity_specs_clear_%s(scope_entry_t* s)\n{\n    s->_entity_specs.%s = NULL;\n    s->_entity_specs.%s = 0;\n}" % (list_name, list_name, num_name)
          print "static inline void symbol_entity_specs_free_%s(scope_entry_t* s)\n{\n    s->_entity_specs.%s = 0;\n    DELETE(s->_entity_specs.%s);\n    s->_entity_specs.%s = NULL;\n}" % (list_name, num_name, list_name, list_name)
          print "static inline void symbol_entity_specs_copy_%s_from(scope_entry_t* dest, scope_entry_t* source)\n{\n    symbol_entity_specs_reserve_%s(dest, source->_entity_specs.%s);\n    memcpy(dest->_entity_specs.%s,\n        source->_entity_specs.%s,\n        dest->_entity_specs.%s\n        * (sizeof (*(dest->_entity_specs.%s))));\n} " % (list_name, list_name, num_name, list_name, list_name, num_name, list_name)
          print ""

    print "static inline void symbol_entity_specs_copy_from(scope_entry_t* dest, scope_entry_t* source)"
    print "{"
    print "    dest->_entity_specs = source->_entity_specs;"
    # Now copy every list
    for l in lines:
      fields = l.split("|");
      (_type,language,name,description) = fields
      if name[0] == "*":
          name = name[1:]
      if _type.startswith("array"):
          type_name = get_up_to_matching_paren(_type[len("array"):])
          field_names = name.split(",")
          if (len(field_names) == 1):
             num_name = "num_" + name
             list_name = name
          elif (len(field_names) == 2):
              num_name = field_names[0]
              list_name = field_names[1]
          else:
              raise Exception("Invalid number of fields in array name. Only 1 or 2 comma-separated are allowed")
          print "    symbol_entity_specs_copy_%s_from(dest, source);" % (list_name)
    print "}"
    print ""
    print "static inline void symbol_entity_specs_free(scope_entry_t* symbol)"
    print "{"
    # Now copy every list
    for l in lines:
      fields = l.split("|");
      (_type,language,name,description) = fields
      if name[0] == "*":
          name = name[1:]
      if _type.startswith("array"):
          type_name = get_up_to_matching_paren(_type[len("array"):])
          field_names = name.split(",")
          if (len(field_names) == 1):
             num_name = "num_" + name
             list_name = name
          elif (len(field_names) == 2):
              num_name = field_names[0]
              list_name = field_names[1]
          else:
              raise Exception("Invalid number of fields in array name. Only 1 or 2 comma-separated are allowed")
          print "    symbol_entity_specs_free_%s(symbol);" % (list_name)
    print "}"

    print "#endif"

def insert_extra_attr_code(_type, name, getter, getter_extra_args):
    _insert_code = []
    if (_type == "integer") :
        _insert_code.append("insert_extra_attr_int(handle, sym, \"%s\", %s(sym%s));" % (name, getter, getter_extra_args))
    elif (_type == "bool"):
        _insert_code.append("insert_extra_attr_int(handle, sym, \"%s\", %s(sym%s));" % (name, getter, getter_extra_args))
    elif (_type == "string"):
        _insert_code.append("insert_extra_attr_string(handle, sym, \"%s\", %s(sym%s));" % (name, getter, getter_extra_args))
    elif (_type == "AST"):
        _insert_code.append("insert_extra_attr_ast(handle, sym, \"%s\", %s(sym%s));" % (name, getter, getter_extra_args))
    elif (_type == "nodecl"):
        _insert_code.append("insert_extra_attr_nodecl(handle, sym, \"%s\", %s(sym%s));" % (name, getter, getter_extra_args))
    elif (_type == "type"):
        _insert_code.append("insert_extra_attr_type(handle, sym, \"%s\", %s(sym%s));" % (name, getter, getter_extra_args))
    elif (_type == "symbol"):
        _insert_code.append("insert_extra_attr_symbol(handle, sym, \"%s\", %s(sym%s));" % (name, getter, getter_extra_args))
    elif (_type.startswith("array")):
        type_name = get_up_to_matching_paren(_type[len("array"):])
        field_names = name.split(",")
        if (len(field_names) == 1):
           num_name = "num_" + name
           list_name = name
        elif (len(field_names) == 2):
            num_name = field_names[0]
            list_name = field_names[1]
        else:
            raise Exception("Invalid number of fields in array name. Only 1 or 2 comma-separated are allowed")
        _insert_code.append("{ int i; for (i = 0; i < symbol_entity_specs_get_%s(sym); i++) {" % (num_name));
        _insert_code.append(
                string.join(
                    insert_extra_attr_code(type_name, list_name, "symbol_entity_specs_get_%s_num" % (list_name), ", i"), "\n")
                );
        _insert_code.append("} }");
    elif (_type.startswith("typeof")):
        type_name = get_up_to_matching_paren(_type[len("typeof"):])
        if type_name == "gcc_attribute_t" :
            _insert_code.append("{");
            _insert_code.append("gcc_attribute_t gcc_attr = %s(sym%s);" % (getter, getter_extra_args));
            _insert_code.append("insert_extra_gcc_attr(handle, sym, \"%s\", &gcc_attr);" % (name))
            _insert_code.append("}")
        elif type_name == "default_argument_info_t*" :
            _insert_code.append("{");
            _insert_code.append("default_argument_info_t* default_arg = %s(sym%s);" % (getter, getter_extra_args))
            _insert_code.append("insert_extra_attr_data(handle, sym, \"%s\", default_arg, "\
                    "insert_default_argument_info_ptr);" % (name));
            _insert_code.append("}")
        elif type_name == "function_parameter_info_t":
            _insert_code.append("function_parameter_info_t function_param = %s(sym%s);" % (getter, getter_extra_args))
            _insert_code.append("insert_extra_function_parameter_info(handle, sym, \"%s\", &function_param);" % (name))
        else:
            sys.stderr.write("%s:%d: warning: typeof '%s' is not handled\n" % (sys.argv[0], lineno(), type_name))
    else:
        pass
    return _insert_code

def get_extra_load_code(_type, num_name, list_name):
    result = []
    if (_type == "symbol"):
        result.append("{")
        result.append("extra_syms_t extra_syms;")
        result.append("memset(&extra_syms, 0, sizeof(extra_syms));")
        result.append("extra_syms.handle = handle;");
        result.append("get_extended_attribute(handle, sym_oid, \"" + list_name + "\", &extra_syms, get_extra_syms);")
        result.append("int i;");
        result.append("for (i = 0; i < extra_syms.num_syms; i++)")
        result.append("{")
        result.append("symbol_entity_specs_append_%s(sym, extra_syms.syms[i]);" % (list_name))
        result.append("}")
        result.append("}")
    elif (_type == "type"):
        result.append("{")
        result.append("extra_types_t extra_types;")
        result.append("memset(&extra_types, 0, sizeof(extra_types));")
        result.append("extra_types.handle = handle;");
        result.append("get_extended_attribute(handle, sym_oid, \"" + list_name + "\", &extra_types, get_extra_types);")
        result.append("int i;");
        result.append("for (i = 0; i < extra_types.num_types; i++)")
        result.append("{")
        result.append("symbol_entity_specs_append_%s(sym, extra_types.types[i]);" % (list_name))
        result.append("}")
        result.append("}")
    elif (_type == "AST"):
        result.append("{")
        result.append("extra_trees_t extra_trees;")
        result.append("memset(&extra_trees, 0, sizeof(extra_trees));")
        result.append("extra_trees.handle = handle;");
        result.append("get_extended_attribute(handle, sym_oid, \"" + list_name + "\", &extra_trees, get_extra_trees);")
        result.append("int i;");
        result.append("for (i = 0; i < extra_trees.num_trees; i++)")
        result.append("{")
        result.append("symbol_entity_specs_append_%s(sym, extra_trees.trees[i]);" % (list_name))
        result.append("}")
        result.append("}")
    elif (_type == "nodecl"):
        result.append("{")
        result.append("extra_nodecls_t extra_nodecls;")
        result.append("memset(&extra_nodecls, 0, sizeof(extra_nodecls));")
        result.append("extra_nodecls.handle = handle;");
        result.append("get_extended_attribute(handle, sym_oid, \"" + list_name + "\", &extra_nodecls, get_extra_nodecls);")
        result.append("int i;");
        result.append("for (i = 0; i < extra_nodecls.num_nodecls; i++)")
        result.append("{")
        result.append("symbol_entity_specs_append_%s(sym, extra_nodecls.nodecls[i]);" % (list_name))
        result.append("}")
        result.append("}")
    elif (_type.startswith("typeof")):
        type_name = get_up_to_matching_paren(_type[len("typeof"):])
        if type_name == "gcc_attribute_t" :
            result.append("{")
            result.append("extra_gcc_attrs_t extra_gcc_attrs;");
            result.append("memset(&extra_gcc_attrs, 0, sizeof(extra_gcc_attrs));");
            result.append("extra_gcc_attrs.handle = handle;");
            result.append("extra_gcc_attrs.symbol = sym;");
            result.append("get_extended_attribute(handle, sym_oid, \"" + list_name + "\", &extra_gcc_attrs, get_extra_gcc_attrs);");
            result.append("}")
        elif type_name == "function_parameter_info_t" :
            result.append("{")
            result.append("extra_gcc_attrs_t extra_gcc_attrs;");
            result.append("memset(&extra_gcc_attrs, 0, sizeof(extra_gcc_attrs));");
            result.append("extra_gcc_attrs.handle = handle;");
            result.append("extra_gcc_attrs.symbol = sym;");
            result.append("get_extended_attribute(handle, sym_oid, \"" + list_name + "\", &extra_gcc_attrs, get_extra_function_parameter_info);");
            result.append("}")
        elif type_name == "default_argument_info_t*" :
            result.append("{")
            result.append("extra_default_argument_info_t extra_default_argument_info;")
            result.append("memset(&extra_default_argument_info, 0, sizeof(extra_default_argument_info_t));")
            result.append("extra_default_argument_info.handle = handle;");
            result.append("extra_default_argument_info.symbol = sym;");
            result.append("get_extended_attribute(handle, sym_oid, \"" + list_name + "\", &extra_default_argument_info, \
                    get_extra_default_argument_info);")
            result.append("}")
        else:
            pass
    else:
        sys.stderr.write("%s:%d: warning: unknown array type '%s'\n" % (sys.argv[0], lineno(), _type))
    return string.join(result, "\n");

def get_load_code(_type, name):
    result = []
    result.append("{");
    if (_type == "integer") :
        result.append("int i;");
        result.append("if (query_contains_field(ncols, names, \"" + name + "\", &i))");
        result.append("{")
        result.append("   symbol_entity_specs_set_%s(sym, safe_atoull(values[i]));" % (name));
        result.append("}")
    elif (_type == "string"):
        result.append("int i;");
        result.append("if (query_contains_field(ncols, names, \"" + name + "\", &i))");
        result.append("{")
        result.append("   symbol_entity_specs_set_%s(sym, uniquestr(values[i]));" % (name));
        result.append("}")
    elif (_type == "AST"):
        result.append("int i;");
        result.append("if (query_contains_field(ncols, names, \"" + name + "\", &i))");
        result.append("{")
        result.append("   symbol_entity_specs_set_%s(sym, load_ast(handle, safe_atoull(values[i])));" % (name))
        result.append("}")
    elif (_type == "nodecl"):
        result.append("int i;");
        result.append("if (query_contains_field(ncols, names, \"" + name + "\", &i))");
        result.append("{")
        result.append("   symbol_entity_specs_set_%s(sym, load_nodecl(handle, safe_atoull(values[i])));" % (name))
        result.append("}")
    elif (_type == "type"):
        result.append("int i;");
        result.append("if (query_contains_field(ncols, names, \"" + name + "\", &i))");
        result.append("{")
        result.append("   symbol_entity_specs_set_%s(sym, load_type(handle, safe_atoull(values[i])));" % (name))
        result.append("}")
    elif (_type == "symbol"):
        result.append("int i;");
        result.append("if (query_contains_field(ncols, names, \"" + name + "\", &i))");
        result.append("{")
        result.append("   symbol_entity_specs_set_%s(sym, load_symbol(handle, safe_atoull(values[i])));" % (name))
        result.append("}")
    elif (_type.startswith("array")):
        type_name = get_up_to_matching_paren(_type[len("array"):])
        field_names = name.split(",")
        if (len(field_names) == 1):
           num_name = "num_" + name
           list_name = name
        elif (len(field_names) == 2):
            num_name = field_names[0]
            list_name = field_names[1]
        else:
            raise Exception("Invalid number of fields in array name. Only 1 or 2 comma-separated are allowed")

        result.append(get_extra_load_code(type_name, num_name, list_name))
    elif (_type.startswith("typeof")):
        type_name = get_up_to_matching_paren(_type[len("typeof"):]).split(",")[0].strip()
        if type_name in ["intent_kind_t", "access_specifier_t", "_size_t"]:
            result.append("int i;");
            result.append("if (query_contains_field(ncols, names, \"" + name + "\", &i))");
            result.append("{")
            result.append("   symbol_entity_specs_set_%s(sym, safe_atoull(values[i]));" % (name));
            result.append("}")
        elif type_name == "simplify_function_t":
            result.append("int i;");
            result.append("if (query_contains_field(ncols, names, \"" + name + "\", &i))");
            result.append("{")
            result.append("   int id = safe_atoull(values[i]);")
            result.append("   simplify_function_t fun = fortran_simplify_function_get_ptr(id);" )
            result.append("   ERROR_CONDITION(fun == NULL && id != 0, " \
                "\"Invalid identifier %d for simplification function.\\nYou may have to rebuild your Fortran modules\", id);")
            result.append("   symbol_entity_specs_set_%s(sym, fortran_simplify_function_get_ptr(safe_atoull(values[i])));" % (name));
            result.append("}")
        else:
            sys.stderr.write("%s:%d: warning: not handling typeof '%s'\n" % (sys.argv[0], lineno(), type_name))
        pass
    elif (_type == "scope"):
        result.append("// Scope is not stored (yet)");
        result.append("symbol_entity_specs_set_%s(sym, CURRENT_COMPILED_FILE->global_decl_context;" % (name));
    elif (_type == "bool"):
        # Booleans are handled apart
        pass
    else :
        sys.stderr.write("%s:%d: warning: not handling '%s'\n" % (sys.argv[0], lineno(), type_name))
        pass
    result.append("}");
    return string.join(result, "\n");


def print_fortran_modules_functions(lines):
    attr_names = []
    sprintf_arguments = []
    _format = []
    _insert_code = [];
    for l in lines:
      fields = l.split("|");
      (_type,language,name,description) = fields
      if (name[0] == "*"):
          continue
      if (_type == "bool"):
          # Booleans are handled different
          pass
      elif (_type == "integer"):
          attr_names.append(name)
          _format.append("%d")
          sprintf_arguments.append("symbol_entity_specs_get_%s(sym)" % (name))
      elif (_type == "AST"):
          attr_names.append(name)
          _format.append("%llu")
          _insert_code.append("    insert_ast(handle, symbol_entity_specs_get_%s(sym));" % (name));
          sprintf_arguments.append("P2ULL(symbol_entity_specs_get_%s(sym))" % (name))
      elif (_type == "nodecl"):
          attr_names.append(name)
          _format.append("%llu")
          _insert_code.append("    insert_nodecl(handle, symbol_entity_specs_get_%s(sym));" % (name));
          sprintf_arguments.append("P2ULL(nodecl_get_ast(symbol_entity_specs_get_%s(sym)))" % (name))
      elif (_type == "type"):
          attr_names.append(name)
          _format.append("%llu")
          _insert_code.append("    insert_type(handle, symbol_entity_specs_get_%s(sym));" % (name));
          sprintf_arguments.append("P2ULL(symbol_entity_specs_get_%s(sym))" % (name))
      elif (_type == "symbol"):
          attr_names.append(name)
          _format.append("%llu")
          _insert_code.append("    insert_symbol(handle, symbol_entity_specs_get_%s(sym));" % (name));
          sprintf_arguments.append("P2ULL(symbol_entity_specs_get_%s(sym))" % (name))
      elif (_type == "string"):
          attr_names.append(name)
          _format.append("%Q")
          sprintf_arguments.append("symbol_entity_specs_get_%s(sym)" % (name))
      elif (_type.startswith("typeof")):
            type_name = get_up_to_matching_paren(_type[len("typeof"):]).split(",")[0].strip()
            if type_name == "intent_kind_t" or type_name == "access_specifier_t":
                attr_names.append(name)
                _format.append("%d")
                sprintf_arguments.append("(int)symbol_entity_specs_get_%s(sym)" % (name))
            elif type_name == "_size_t":
                attr_names.append(name)
                _format.append("%llu")
                sprintf_arguments.append("(unsigned long long)symbol_entity_specs_get_%s(sym)" % (name))
            elif type_name == "simplify_function_t":
                attr_names.append(name);
                _format.append("%d")
                sprintf_arguments.append("fortran_simplify_function_get_id(symbol_entity_specs_get_%s(sym))" % (name))
            else:
                sys.stderr.write("%s:%d: warning: not handling typeof '%s'\n" % (sys.argv[0], lineno(), type_name))
      else:
          pass
    print "#ifndef FORTRAN03_MODULES_BITS_H"
    print "#define FORTRAN03_MODULES_BITS_H"
    print ""
    print "static const char * attr_field_names = \"" + string.join(attr_names, ", ") + "\";";
    print "static char * symbol_get_attribute_values(sqlite3* handle, scope_entry_t* sym)"
    print "{"
    print "    const char *format = \"" + string.join(_format, ", ") + "\";"
    print ""
    print string.join(_insert_code, "\n");
    print ""
    print "    char * result = sqlite3_mprintf(format, " + string.join(sprintf_arguments, ", ") + ");"
    print "    return result;"
    print "}"
    _extra_attr_code = []
    for l in lines:
      fields = l.split("|");
      (_type,language,name,description) = fields
      if name[0] == "*":
          continue;
      if (_type.startswith("array")):
          _extra_attr_code = _extra_attr_code + insert_extra_attr_code(_type, name, "", "")
      else:
           pass
    print "static void insert_extended_attributes(sqlite3* handle, scope_entry_t* sym)"
    print "{"
    print string.join(_extra_attr_code, "\n");
    print "}"
    print ""
    print "static void get_extra_attributes(sqlite3* handle, int ncols, char **values, char **names, sqlite3_uint64 sym_oid, scope_entry_t* sym)"
    print "{"
    for l in lines:
      fields = l.split("|");
      (_type,language,name,description) = fields
      if name[0] == "*":
          continue;
      print get_load_code(_type, name)
    print "}"

    print "typedef struct module_packed_bits_tag"
    print "{"
    for l in lines:
      fields = l.split("|");
      (_type,language,name,description) = fields
      if name[1] == "*":
          continue;
      if _type == "bool":
          print "_Bool %s:1;" % (name)
    print "} module_packed_bits_t;"

    print "static module_packed_bits_t synthesize_packed_bits(scope_entry_t* sym)"
    print "{"
    print "module_packed_bits_t result;"
    print "memset(&result, 0, sizeof(result));"
    for l in lines:
      fields = l.split("|");
      (_type,language,name,description) = fields
      if name[1] == "*":
          continue;
      if _type == "bool":
          print "result.%s = symbol_entity_specs_get_%s(sym);" % (name, name)
    print "return result;"
    print "}"

    # print "static void unpack_bits(entity_specifiers_t *_entity_specs, module_packed_bits_t bitpack)"
    # print "{"
    # for l in lines:
    #   fields = l.split("|");
    #   (_type,language,name,description) = fields
    #   if name[1] == "*":
    #       continue;
    #   if _type == "bool":
    #       print "_entity_specs->%s = bitpack.%s;" % (name, name)
    # print "}"

    print "static void unpack_bits(scope_entry_t *sym, module_packed_bits_t bitpack)"
    print "{"
    for l in lines:
      fields = l.split("|");
      (_type,language,name,description) = fields
      if name[1] == "*":
          continue;
      if _type == "bool":
          print "symbol_entity_specs_set_%s(sym, bitpack.%s);" % (name, name)
    print "}"

    print "#endif // FORTRAN03_MODULES_BITS_H"


def print_deep_copy_entity_specs(lines):
    print "// DO NOT MODIFY THIS FILE."
    print "// It will be overwritten when gen-symbols-attrs.py or cxx-entity_specs.def are modified"
    print "#include \"cxx-scope.h\""
    print "#include \"cxx-nodecl-deep-copy.h\""
    print "#include \"cxx-typeutils.h\""
    print "#include \"string_utils.h\""

    print """
    void symbol_deep_copy_entity_specs(scope_entry_t* dest, scope_entry_t* source,
             const decl_context_t* decl_context, symbol_map_t* symbol_map,
             nodecl_deep_copy_map_t* nodecl_deep_copy_map,
             symbol_deep_copy_map_t* symbol_deep_copy_map)
    {
    """
    for l in lines:
      fields = l.split("|");
      (_type,language,name,description) = fields
      if (name[0] == "*"):
          name = name[1:]
          continue
      if name == "function_code":
          print "// We do not copy function code!"
          continue
      if _type in ["bool", "integer"]:
          print "symbol_entity_specs_set_%s(dest, symbol_entity_specs_get_%s(source));" % (name, name)
      elif (_type == "scope"):
          print "symbol_entity_specs_set_%s(dest, decl_context);"
      elif (_type == "nodecl"):
          print "symbol_entity_specs_set_%s(dest, nodecl_deep_copy_compute_maps(symbol_entity_specs_get_%s(source), decl_context, symbol_map, nodecl_deep_copy_map, symbol_deep_copy_map));" % (name, name)
      elif (_type == "type"):
          print "symbol_entity_specs_set_%s(dest, type_deep_copy_compute_maps(symbol_entity_specs_get_%s(source), /* symbol dest */ NULL, decl_context, symbol_map, nodecl_deep_copy_map, symbol_deep_copy_map));" % (name, name)
      elif (_type == "symbol"):
          print "symbol_entity_specs_set_%s(dest, symbol_map->map(symbol_map, symbol_entity_specs_get_%s(source)));" % (name, name)
      elif (_type == "string"):
          print "symbol_entity_specs_set_%s(dest, symbol_entity_specs_get_%s(source));" % (name, name)
      elif (_type.startswith("typeof")):
            type_name = get_up_to_matching_paren(_type[len("typeof"):]).split(",")[0].strip()
            if type_name in ["intent_kind_t", "access_specifier_t", "_size_t", "simplify_function_t"]:
                print "symbol_entity_specs_set_%s(dest, symbol_entity_specs_get_%s(source));" % (name, name)
            else:
                sys.stderr.write("%s:%d: warning: not handling typeof '%s'\n" % (sys.argv[0], lineno(), type_name))
      elif (_type.startswith("array")):
          type_name = get_up_to_matching_paren(_type[len("array"):])
          field_names = name.split(",")
          if (len(field_names) == 1):
             num_name = "num_" + name
             list_name = name
          elif (len(field_names) == 2):
              num_name = field_names[0]
              list_name = field_names[1]
          else:
              raise Exception("Invalid number of fields in array name. Only 1 or 2 comma-separated are allowed")
          if type_name.startswith("typeof"):
              type_name = get_up_to_matching_paren(type_name[len("typeof"):]).split(",")[0].strip()
          print "{"
          print "symbol_entity_specs_free_%s(dest);" % (list_name)
          print "int i, N = symbol_entity_specs_get_%s(source);" % (num_name)
          print "for (i = 0; i < N; i++)"
          print "{"
          if type_name == "symbol":
              print "scope_entry_t* copied = symbol_map->map(symbol_map, symbol_entity_specs_get_%s_num(source, i));" % (list_name)
              print "symbol_entity_specs_add_%s(dest, copied);" % (list_name);
          elif type_name == "type":
              print "type_t* copied = type_deep_copy_compute_maps(symbol_entity_specs_get_%s_num(source, i), /* dest */ NULL, decl_context, symbol_map, nodecl_deep_copy_map, symbol_deep_copy_map);" % (list_name)
              print "symbol_entity_specs_add_%s(dest, copied);" % (list_name);
          elif type_name == "default_argument_info_t*":
                  print "default_argument_info_t* source_default_arg = symbol_entity_specs_get_%s_num(source, i);" % (list_name)
                  print "default_argument_info_t* copied = NULL;"
                  print "if (source_default_arg != NULL)"
                  print "{"
                  print "  copied = NEW0(default_argument_info_t);"
                  print "  copied->argument = nodecl_deep_copy_compute_maps(source_default_arg->argument, decl_context, symbol_map, nodecl_deep_copy_map, symbol_deep_copy_map);"
                  print "  copied->context = decl_context;"
                  print "}"
                  print "symbol_entity_specs_add_%s(dest, copied);" % (list_name)
          elif type_name == "gcc_attribute_t":
              print "gcc_attribute_t source_gcc_attr = symbol_entity_specs_get_%s_num(source, i);" % (list_name)
              print "gcc_attribute_t copied;"
              print "copied.attribute_name = source_gcc_attr.attribute_name;"
              print "copied.expression_list = nodecl_deep_copy_compute_maps(source_gcc_attr.expression_list, decl_context, symbol_map, nodecl_deep_copy_map, symbol_deep_copy_map);"
              print "symbol_entity_specs_add_%s(dest, copied);" % (list_name)
          elif type_name == "function_parameter_info_t":
              print "function_parameter_info_t param_info = symbol_entity_specs_get_%s_num(source, i);" % (list_name)
              print "param_info.function = symbol_map->map(symbol_map, param_info.function);"
              print "symbol_entity_specs_add_%s(dest, param_info);" % (list_name)
          else:
              sys.stderr.write("%s:%d: warning: not handling type array of type '%s'\n" % (sys.argv[0], lineno(), _type))
          print "}"
          print "}"
      else:
          sys.stderr.write("%s:%d: warning: not handling type '%s'\n" % (sys.argv[0], lineno(), _type))
    print """
    }
    """

lines = loadlines(f)
check_file(lines)

print "/* This file has been generated by gen-symbols-attrs.py. */" 
print "/* Do not modify it or you'll get what you deserve */"

if op == "entity_specifiers":
    print_entity_specifiers(lines)
elif op == "getters_and_setters":
    print_getters_setters(lines)
elif op == "fortran_modules":
    print_fortran_modules_functions(lines)
elif op == "c_deep_copy_entity_specs":
    print_deep_copy_entity_specs(lines)
else:
    raise Exception("Invalid operation %s" % (op))

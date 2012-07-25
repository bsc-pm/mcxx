#!/usr/bin/python

import sys
import string

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


def print_type_and_name(_type, name):
    _type = _type.strip(" \n")
    if (_type == "bool"):  
        return [("bool", name, ":1")]
    elif (_type == "integer"):
        return [("int", name, "")]
    elif (_type == "AST"):
        return [("AST", name, "")]
    elif (_type == "nodecl"):
        return [("nodecl_t", name, "")]
    elif (_type == "type"):
        return [("type_t*", name, "")]
    elif (_type == "string"):
        return [("const char*", name, "")]
    elif (_type == "symbol"):
        return [("scope_entry_t*", name, "")]
    elif (_type == "scope"):
        return [("decl_context_t", name, "")]
    elif (_type.startswith("typeof")):
        type_name = get_up_to_matching_paren(_type[len("typeof"):])
        return [(type_name, name, "")]
    elif (_type.startswith("pointer")):
        type_name = get_up_to_matching_paren(_type[len("pointer"):])
        (t, n, s) = print_type_and_name(type_name, name)[0]
        return [(t + "*", n, s)]
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
    elif (_type.startswith("static_array")):
        (type_name, size) = get_up_to_matching_paren(_type[len("static_array"):]).split(",")
        field_names = name.split(",")
        if (len(field_names) == 1):
           num_name = "num_" + name
           list_name = name
        elif (len(field_names) == 2):
            num_name = field_names[0]
            list_name = field_names[1]
        else:
            raise Exception("Invalid number of fields in static_array name. Only 1 or 2 comma-separated are allowed")
        type_name = type_name.strip(" \n")
        size = size.strip(" \n")                                                                          
        (t, n, s) = print_type_and_name(type_name, list_name)[0]
        return print_type_and_name("integer", num_name) + [(t, n, s + "[" + size + "]")] 
    else:
        raise Exception("Invalid type %s" % (_type))

def print_entity_specifiers(lines):
    print """
#ifndef CXX_ENTITY_SPECIFIERS_H
#define CXX_ENTITY_SPECIFIERS_H

#include <stdbool.h>

// Include this file only from cxx-scope-decls.h and not from anywhere else

typedef struct entity_specifiers_tag\n{"""
    indent = " " * 4
    current_language = "all"
    for l in lines:
      fields = l.split("|");
      (_type,language,name,description) = fields
      if name[0] == "*":
          name = name[1:]

      language = language.strip(" \n")
      if (language != current_language) :
          current_language = language
      descr = description.strip(" \n")
      if (descr):
         print indent + "// " + description.rstrip(" \n")
      decls = print_type_and_name(_type, name)
      for d in decls:
        (typename, name, suffix) = d
        print indent + typename + " " + name + suffix + ";"
    print "} entity_specifiers_t;"
    print ""
    print "#endif"

def insert_extra_attr_code(_type, name, suffix):
    _insert_code = []
    if (_type == "integer") :
        _insert_code.append("insert_extra_attr_int(handle, sym, \"" + name + "\", sym->entity_specs." + name + suffix + ");")
    elif (_type == "bool"):
        _insert_code.append("insert_extra_attr_int(handle, sym, \"" + name + "\", sym->entity_specs." + name + suffix + ");")
    elif (_type == "string"):
        _insert_code.append("insert_extra_attr_string(handle, sym, \"" + name + "\", sym->entity_specs." + name + suffix + ");")
    elif (_type == "AST"):
        _insert_code.append("insert_extra_attr_ast(handle, sym, \"" + name + "\", sym->entity_specs." + name + suffix + ");")
    elif (_type == "nodecl"):
        _insert_code.append("insert_extra_attr_nodecl(handle, sym, \"" + name + "\", sym->entity_specs." + name + suffix + ");")
    elif (_type == "type"):
        _insert_code.append("insert_extra_attr_type(handle, sym, \"" + name + "\", sym->entity_specs." + name + suffix + ");")
    elif (_type == "symbol"):
        _insert_code.append("insert_extra_attr_symbol(handle, sym, \"" + name + "\", sym->entity_specs." + name + suffix + ");")
    elif (_type.startswith("array") or _type.startswith("static_array")):
        if _type.startswith("array"):
            type_name = get_up_to_matching_paren(_type[len("array"):])
        elif _type.startswith("static_array"):
            type_name = get_up_to_matching_paren(_type[len("static_array"):])
        field_names = name.split(",")
        if (len(field_names) == 1):
           num_name = "num_" + name
           list_name = name
        elif (len(field_names) == 2):
            num_name = field_names[0]
            list_name = field_names[1]
        else:
            raise Exception("Invalid number of fields in array name. Only 1 or 2 comma-separated are allowed")
        # _insert_code.append(insert_extra_attr_code("integer", num_name, "")[0])
        _insert_code.append("{ int i; for (i = 0; i < sym->entity_specs." + num_name + "; i++) {");
        _insert_code = _insert_code + insert_extra_attr_code(type_name, list_name, "[i]");
        _insert_code.append("} }");
    elif (_type.startswith("typeof")):
        type_name = get_up_to_matching_paren(_type[len("typeof"):])
        if type_name == "gather_gcc_attribute_t" :
            _insert_code.append("insert_extra_gcc_attr(handle, sym, \"" + name + "\", &(sym->entity_specs." + name + suffix + "));")
        elif type_name == "default_argument_info_t*" :
            _insert_code.append("insert_extra_attr_data(handle, sym, \"" + name + "\", sym->entity_specs." + name + suffix + ", "\
                    "insert_default_argument_info_ptr);");
        else:
            sys.stderr.write("%s: info: typeof '%s' is not handled\n" % (sys.argv[0], type_name))
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
        result.append("sym->entity_specs." + num_name + " = extra_syms.num_syms;")
        result.append("sym->entity_specs." + list_name + " = extra_syms.syms;")
        result.append("}")
    elif (_type == "type"):
        result.append("{")
        result.append("extra_types_t extra_types;")
        result.append("memset(&extra_types, 0, sizeof(extra_types));")
        result.append("extra_types.handle = handle;");
        result.append("get_extended_attribute(handle, sym_oid, \"" + list_name + "\", &extra_types, get_extra_types);")
        result.append("sym->entity_specs." + num_name + " = extra_types.num_types;")
        result.append("sym->entity_specs." + list_name + " = extra_types.types;")
        result.append("}")
    elif (_type == "AST"):
        result.append("{")
        result.append("extra_trees_t extra_trees;")
        result.append("memset(&extra_trees, 0, sizeof(extra_trees));")
        result.append("extra_trees.handle = handle;");
        result.append("get_extended_attribute(handle, sym_oid, \"" + list_name + "\", &extra_trees, get_extra_trees);")
        result.append("sym->entity_specs." + num_name + " = extra_trees.num_trees;")
        result.append("sym->entity_specs." + list_name + " = extra_trees.trees;")
        result.append("}")
    elif (_type == "nodecl"):
        result.append("{")
        result.append("extra_nodecls_t extra_nodecls;")
        result.append("memset(&extra_nodecls, 0, sizeof(extra_nodecls));")
        result.append("extra_nodecls.handle = handle;");
        result.append("get_extended_attribute(handle, sym_oid, \"" + list_name + "\", &extra_nodecls, get_extra_nodecls);")
        result.append("sym->entity_specs." + num_name + " = extra_nodecls.num_nodecls;")
        result.append("sym->entity_specs." + list_name + " = extra_nodecls.nodecls;")
        result.append("}")
    elif (_type.startswith("typeof")):
        type_name = get_up_to_matching_paren(_type[len("typeof"):])
        if type_name == "gather_gcc_attribute_t" :
            result.append("{")
            result.append("extra_gcc_attrs_t extra_gcc_attrs;");
            result.append("memset(&extra_gcc_attrs, 0, sizeof(extra_gcc_attrs));");
            result.append("extra_gcc_attrs.handle = handle;");
            result.append("get_extended_attribute(handle, sym_oid, \"" + list_name + "\", &extra_gcc_attrs, get_extra_gcc_attrs);");
            result.append("}")
            # No need to copy back to the symbol
            pass
        elif type_name == "default_argument_info_t*" :
            result.append("{")
            result.append("extra_default_argument_info_t extra_default_argument_info;")
            result.append("memset(&extra_default_argument_info, 0, sizeof(extra_default_argument_info_t));")
            result.append("extra_default_argument_info.handle = handle;");
            result.append("get_extended_attribute(handle, sym_oid, \"" + list_name + "\", &extra_default_argument_info, \
                    get_extra_default_argument_info);")
            result.append("}")
            pass
    else:
        sys.stderr.write("%s: warning: unknown array type '%s'\n" % (sys.argv[0], _type))
    return string.join(result, "\n");

def get_load_code(_type, name):
    result = []
    result.append("{");
    if (_type == "integer") :
        result.append("int i;");
        result.append("if (query_contains_field(ncols, names, \"" + name + "\", &i))");
        result.append("{")
        result.append("   sym->entity_specs." + name + " = safe_atoull(values[i]);")
        result.append("}")
    elif (_type == "string"):
        result.append("int i;");
        result.append("if (query_contains_field(ncols, names, \"" + name + "\", &i))");
        result.append("{")
        result.append("   sym->entity_specs." + name + " = uniquestr(values[i]);")
        result.append("}")
    elif (_type == "AST"):
        result.append("int i;");
        result.append("if (query_contains_field(ncols, names, \"" + name + "\", &i))");
        result.append("{")
        result.append("   sym->entity_specs." + name + " = load_ast(handle, safe_atoull(values[i]));")
        result.append("}")
    elif (_type == "nodecl"):
        result.append("int i;");
        result.append("if (query_contains_field(ncols, names, \"" + name + "\", &i))");
        result.append("{")
        result.append("   sym->entity_specs." + name + " = load_nodecl(handle, safe_atoull(values[i]));")
        result.append("}")
    elif (_type == "type"):
        result.append("int i;");
        result.append("if (query_contains_field(ncols, names, \"" + name + "\", &i))");
        result.append("{")
        result.append("   sym->entity_specs." + name + " = load_type(handle, safe_atoull(values[i]));")
        result.append("}")
    elif (_type == "symbol"):
        result.append("int i;");
        result.append("if (query_contains_field(ncols, names, \"" + name + "\", &i))");
        result.append("{")
        result.append("   sym->entity_specs." + name + " = load_symbol(handle, safe_atoull(values[i]));")
        result.append("}")
    elif (_type.startswith("array") or _type.startswith("static_array")):
        if _type.startswith("array"):
            type_name = get_up_to_matching_paren(_type[len("array"):])
        elif _type.startswith("static_array"):
            type_name = get_up_to_matching_paren(_type[len("static_array"):])
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
        type_name = get_up_to_matching_paren(_type[len("typeof"):])
        if type_name == "intent_kind_t" or type_name == "access_specifier_t" or type_name == "_size_t":
            result.append("int i;");
            result.append("if (query_contains_field(ncols, names, \"" + name + "\", &i))");
            result.append("{")
            result.append("   sym->entity_specs." + name + " = safe_atoull(values[i]);")
            result.append("}")
        else:
            sys.stderr.write("%s: info: not handling typeof '%s'\n" % (sys.argv[0], type_name))
        pass
    elif (_type == "scope"):
        result.append("// Scope is not stored (yet)");
        result.append("sym->entity_specs." + name + " = CURRENT_COMPILED_FILE->global_decl_context;")
    elif (_type == "bool"):
        # Booleans are handled apart
        pass
    else :
        sys.stderr.write("%s: info: not handling '%s'\n" % (sys.argv[0], type_name))
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
          sprintf_arguments.append("sym->entity_specs.%s" % (name))
      elif (_type == "AST"):
          attr_names.append(name)
          _format.append("%llu")
          _insert_code.append("    insert_ast(handle, sym->entity_specs." + name + ");");
          sprintf_arguments.append("P2ULL(sym->entity_specs.%s)" % (name))
      elif (_type == "nodecl"):
          attr_names.append(name)
          _format.append("%llu")
          _insert_code.append("    insert_nodecl(handle, sym->entity_specs." + name + ");");
          sprintf_arguments.append("P2ULL(nodecl_get_ast(sym->entity_specs.%s))" % (name))
      elif (_type == "type"):
          attr_names.append(name)
          _format.append("%llu")
          _insert_code.append("    insert_type(handle, sym->entity_specs." + name + ");");
          sprintf_arguments.append("P2ULL(sym->entity_specs.%s)" % (name))
      elif (_type == "symbol"):
          attr_names.append(name)
          _format.append("%llu")
          _insert_code.append("    insert_symbol(handle, sym->entity_specs." + name + ");");
          sprintf_arguments.append("P2ULL(sym->entity_specs.%s)" % (name))
      elif (_type == "string"):
          attr_names.append(name)
          _format.append("%Q")
          sprintf_arguments.append("sym->entity_specs.%s" % (name))
      elif (_type.startswith("typeof")):
            type_name = get_up_to_matching_paren(_type[len("typeof"):])
            if type_name == "intent_kind_t" or type_name == "access_specifier_t":
                attr_names.append(name)
                _format.append("%d")
                sprintf_arguments.append("(int)(sym->entity_specs.%s)" % (name))
            elif type_name == "_size_t":
                attr_names.append(name)
                _format.append("%llu")
                sprintf_arguments.append("(unsigned long long)(sym->entity_specs.%s)" % (name))
            else:
                sys.stderr.write("%s: info: not handling typeof '%s'\n" % (sys.argv[0], type_name))
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
      if (_type.startswith("array") or _type.startswith("static_array")):
          _extra_attr_code = _extra_attr_code + insert_extra_attr_code(_type, name, "")
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
          print "result.%s = sym->entity_specs.%s;" % (name, name)
    print "return result;"
    print "}"

    print "static void unpack_bits(scope_entry_t* sym, module_packed_bits_t bitpack)"
    print "{"
    for l in lines:
      fields = l.split("|");
      (_type,language,name,description) = fields
      if name[1] == "*":
          continue;
      if _type == "bool":
          print "sym->entity_specs.%s = bitpack.%s;" % (name, name)
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
    void symbol_deep_copy_entity_specs(scope_entry_t* dest, scope_entry_t* source, decl_context_t decl_context, symbol_map_t* symbol_map)
    {
    """
    for l in lines:
      fields = l.split("|");
      (_type,language,name,description) = fields
      if (name[0] == "*"):
          name = name[1:]
          continue
      if _type in ["bool", "integer"]:
          print "dest->entity_specs.%s = source->entity_specs.%s;" % (name, name)
      elif (_type == "scope"):
          print "dest->entity_specs.%s = decl_context;" % (name)
      elif (_type == "nodecl"):
          print "dest->entity_specs.%s = nodecl_deep_copy(source->entity_specs.%s, decl_context, symbol_map);" % (name, name)
      elif (_type == "type"):
          print "dest->entity_specs.%s = type_deep_copy(source->entity_specs.%s, decl_context, symbol_map);" % (name, name)
      elif (_type == "symbol"):
          print "dest->entity_specs.%s = symbol_map->map(symbol_map, source->entity_specs.%s);" % (name, name)
      elif (_type == "string"):
          print "dest->entity_specs.%s = source->entity_specs.%s;" % (name, name)
      elif (_type.startswith("typeof")):
            type_name = get_up_to_matching_paren(_type[len("typeof"):])
            if type_name in ["intent_kind_t", "access_specifier_t", "_size_t"]:
                print "dest->entity_specs.%s = source->entity_specs.%s;" % (name, name)
            else:
                sys.stderr.write("%s: warning: not handling typeof '%s'\n" % (sys.argv[0], type_name))
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
          if type_name.startswith("typeof"):
              type_name = get_up_to_matching_paren(type_name[len("typeof"):])
          print "{"
          print "int i, N = source->entity_specs.%s;" % (num_name)
          print "for (i = 0; i < N; i++)"
          print "{"
          if type_name == "symbol":
              print "scope_entry_t* copied = symbol_map->map(symbol_map, source->entity_specs.%s[i]);" % (list_name)
              print "P_LIST_ADD(dest->entity_specs.%s, dest->entity_specs.%s, copied);" % (list_name, num_name)
          elif type_name == "type":
              print "type_t* copied = type_deep_copy(source->entity_specs.%s[i], decl_context, symbol_map);" % (list_name)
              print "P_LIST_ADD(dest->entity_specs.%s, dest->entity_specs.%s, copied);" % (list_name, num_name)
          elif type_name == "default_argument_info_t*":
                  print "default_argument_info_t* source_default_arg = source->entity_specs.%s[i];" % (list_name)
                  print "default_argument_info_t* copied = NULL;"
                  print "if (source_default_arg != NULL)"
                  print "{"
                  print "  copied = calloc(1, sizeof(*copied));"
                  print "  copied->argument = nodecl_deep_copy(source_default_arg->argument, decl_context, symbol_map);"
                  print "  copied->context = decl_context;"
                  print "}"
                  print "P_LIST_ADD(dest->entity_specs.%s, dest->entity_specs.%s, copied);" % (list_name, num_name)
          elif type_name == "gather_gcc_attribute_t":
              print "gather_gcc_attribute_t source_gcc_attr = source->entity_specs.%s[i];" % (list_name)
              print "gather_gcc_attribute_t copied;"
              print "copied.attribute_name = source_gcc_attr.attribute_name;"
              print "copied.expression_list = nodecl_deep_copy(source_gcc_attr.expression_list, decl_context, symbol_map);"
              print "P_LIST_ADD(dest->entity_specs.%s, dest->entity_specs.%s, copied);" % (list_name, num_name)
          else:
              sys.stderr.write("%s: warning: not handling type array of type '%s'\n" % (sys.argv[0], _type))
          print "}"
          print "}"
      else:
          sys.stderr.write("%s: warning: not handling type '%s'\n" % (sys.argv[0], _type))
    print """
    }
    """

lines = loadlines(f)
check_file(lines)

print "/* This file has been generated by gen-symbols-attrs.py. */" 
print "/* Do not modify it or you'll get what you deserve */"

if op == "entity_specifiers":
    print_entity_specifiers(lines)
elif op == "fortran_modules":
    print_fortran_modules_functions(lines)
elif op == "c_deep_copy_entity_specs":
    print_deep_copy_entity_specs(lines)
else:
    raise Exception("Invalid operation %s" % (op))

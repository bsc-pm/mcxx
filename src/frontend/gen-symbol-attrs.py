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
        return [("char", name, ":1")]
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
          if (current_language == "all" and language == "fortran"):
              print "#ifdef FORTRAN_SUPPORT"
          elif (current_language == "fortran" and language == "all"):
              print "#endif // FORTRAN_SUPPORT"
          else: 
              raise Exception("Invalid sequence of languages from %s -> %s" % (current_language, language))
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
    if (_type == "integer" or _type == "bool") :
        result.append("int i;");
        result.append("if (query_contains_field(ncols, names, \"" + name + "\", &i))");
        result.append("{")
        result.append("   sym->entity_specs." + name + " = safe_atoll(values[i]);")
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
        result.append("   sym->entity_specs." + name + " = load_ast(handle, safe_atoll(values[i]));")
        result.append("}")
    elif (_type == "nodecl"):
        result.append("int i;");
        result.append("if (query_contains_field(ncols, names, \"" + name + "\", &i))");
        result.append("{")
        result.append("   sym->entity_specs." + name + " = load_nodecl(handle, safe_atoll(values[i]));")
        result.append("}")
    elif (_type == "type"):
        result.append("int i;");
        result.append("if (query_contains_field(ncols, names, \"" + name + "\", &i))");
        result.append("{")
        result.append("   sym->entity_specs." + name + " = load_type(handle, safe_atoll(values[i]));")
        result.append("}")
    elif (_type == "symbol"):
        result.append("int i;");
        result.append("if (query_contains_field(ncols, names, \"" + name + "\", &i))");
        result.append("{")
        result.append("   sym->entity_specs." + name + " = load_symbol(handle, safe_atoll(values[i]));")
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
            result.append("   sym->entity_specs." + name + " = safe_atoll(values[i]);")
            result.append("}")
        else:
            sys.stderr.write("%s: info: not handling typeof '%s'\n" % (sys.argv[0], type_name))
        pass
    elif (_type == "scope"):
        result.append("// Scope is not stored (yet)");
        result.append("sym->entity_specs." + name + " = CURRENT_COMPILED_FILE->global_decl_context;")
    else :
        sys.stderr.write("%s: info: not handling '%s'\n" % (sys.argv[0], type_name))
        pass
    result.append("}");
    return string.join(result, "\n");


def print_fortran_modules_functions(lines):
    attr_names = []
    _format = []
    _insert_code = [];
    for l in lines:
      fields = l.split("|");
      (_type,language,name,description) = fields
      if (name[0] == "*"):
          continue
      if (_type == "bool"):
          attr_names.append(name)
          _format.append("%d")
      elif (_type == "integer"):
          attr_names.append(name)
          _format.append("%d")
      elif (_type == "AST"):
          attr_names.append(name)
          _format.append("%lld")
          _insert_code.append("    insert_ast(handle, sym->entity_specs." + name + ");");
      elif (_type == "nodecl"):
          attr_names.append(name)
          _format.append("%lld")
          _insert_code.append("    insert_nodecl(handle, sym->entity_specs." + name + ");");
      elif (_type == "type"):
          attr_names.append(name)
          _format.append("%lld")
          _insert_code.append("    insert_type(handle, sym->entity_specs." + name + ");");
      elif (_type == "symbol"):
          attr_names.append(name)
          _format.append("%lld")
          _insert_code.append("    insert_symbol(handle, sym->entity_specs." + name + ");");
      elif (_type == "string"):
          attr_names.append(name)
          _format.append("%Q")
      elif (_type.startswith("typeof")):
            type_name = get_up_to_matching_paren(_type[len("typeof"):])
            if type_name == "intent_kind_t" or type_name == "access_specifier_t":
                attr_names.append(name)
                _format.append("%d")
            elif type_name == "_size_t":
                attr_names.append(name)
                _format.append("%lld")
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
    print "    char * result = sqlite3_mprintf(format, " + string.join(map(lambda x : "sym->entity_specs." + x, attr_names), ", ") + ");"
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
    print "static void get_extra_attributes(sqlite3* handle, int ncols, char **values, char **names, sqlite3_int64 sym_oid, scope_entry_t* sym)"
    print "{"
    for l in lines:
      fields = l.split("|");
      (_type,language,name,description) = fields
      if name[0] == "*":
          continue;
      print get_load_code(_type, name)
    print "}"
    print "#endif // FORTRAN03_MODULES_BITS_H"



lines = loadlines(f)
check_file(lines)

print "/* This file has been generated by gen-symbols-attrs.py. */" 
print "/* Do not modify it or you'll get what you deserve */"

if op == "entity_specifiers":
    print_entity_specifiers(lines)
elif op == "fortran_modules":
    print_fortran_modules_functions(lines)
else:
    raise Exception("Invalid operation %s" % (op))

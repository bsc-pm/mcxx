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
        _insert_code.append(insert_extra_attr_code("integer", num_name, "")[0])
        _insert_code.append("{ int i; for (i = 0; i < sym->entity_specs." + num_name + "; i++) {");
        _insert_code = _insert_code + insert_extra_attr_code(type_name, list_name, "[i]");
        _insert_code.append("} }");
    elif (_type.startswith("typeof")):
        type_name = get_up_to_matching_paren(_type[len("typeof"):])
        if type_name == "gather_gcc_attribute_t" :
            pass
        elif type_name == "default_argument_info_t*" :
            pass
        else:
            sys.stderr.write("%s: warning: unknown typeof '%s'\n" % (sys.argv[0], type_name))
    else:
        pass
    return _insert_code


def print_fortran_modules_functions(lines):
    attr_names = []
    _format = []
    _insertion_code = [];
    for l in lines:
      fields = l.split("|");
      (_type,language,name,description) = fields
      if (_type == "bool"):
          attr_names.append(name)
          _format.append("%d")
      elif (_type == "integer"):
          attr_names.append(name)
          _format.append("%d")
      elif (_type == "AST"):
          attr_names.append(name)
          _format.append("%p")
          _insertion_code.append("    insert_ast(handle, sym->entity_specs." + name + ");");
      elif (_type == "type"):
          attr_names.append(name)
          _format.append("%p")
          _insertion_code.append("    insert_type(handle, sym->entity_specs." + name + ");");
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
                sys.stderr.write("%s: warning: unknown typeof '%s'\n" % (sys.argv[0], type_name))
      else:
          pass
    print "#ifndef FORTRAN03_MODULES_BITS_H"
    print "#define FORTRAN03_MODULES_BITS_H"
    print ""
    print "static const char * attr_field_names = \"" + string.join(attr_names, ", ") + "\";";
    print "static char * symbol_get_attribute_values(storage_handle_t handle, scope_entry_t* sym)"
    print "{"
    print "    const char *format = \"" + string.join(_format, ", ") + "\";"
    print ""
    print string.join(_insertion_code, "\n");
    print ""
    print "    char * result = sqlite3_mprintf(format, " + string.join(map(lambda x : "sym->entity_specs." + x, attr_names), ", ") + ");"
    print "    return result;"
    print "}"
    _extra_attr_code = []
    for l in lines:
      fields = l.split("|");
      (_type,language,name,description) = fields
      if (_type.startswith("array")):
          _extra_attr_code = _extra_attr_code + insert_extra_attr_code(_type, name, "")
      else:
           pass
    print "static void insert_extended_attributes(storage_handle_t handle, scope_entry_t* sym)"
    print "{"
    print string.join(_extra_attr_code, "\n");
    print "}"
    print ""
    print "#endif // FORTRAN03_MODULES_BITS_H"



lines = loadlines(f)
check_file(lines)

if op == "entity_specifiers":
    print_entity_specifiers(lines)
elif op == "fortran_modules":
    print_fortran_modules_functions(lines)
else:
    raise Exception("Invalid operation %s" % (op))

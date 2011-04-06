#!/usr/bin/python

import sys

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
        raise BaseException, "Invalid number of fields"

def get_up_to_matching_paren(s):
    if (s[0] != '('):
        raise BaseException, "String must start with ("
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
    raise BaseException, "No matching left parent"


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
            raise BaseException("Invalid number of fields in array name. Only 1 or 2 comma-separated are allowed")
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
            raise BaseException("Invalid number of fields in static_array name. Only 1 or 2 comma-separated are allowed")
        type_name = type_name.strip(" \n")
        size = size.strip(" \n")                                                                          
        (t, n, s) = print_type_and_name(type_name, list_name)[0]
        return print_type_and_name("integer", num_name) + [(t, n, s + "[" + size + "]")] 
    else:
        raise BaseException("Invalid type %s" % (_type))

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
              raise BaseException("Invalid sequence of languages from %s -> %s" % (current_language, language))
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

lines = loadlines(f)
check_file(lines)

if op == "entity_specifiers":
    print_entity_specifiers(lines)
else:
    raise BaseException("Invalid operation %s" % (op))

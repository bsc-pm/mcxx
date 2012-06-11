#!/usr/bin/python


# complex_types = [ \
#             ("get_complex_type(get_float_type())", "cfloat"), \
#             ("get_complex_type(get_double_type())", "cdouble"), \
#             ("get_complex_type(get_long_double_type())", "cldouble") \
#             ]

float_types = [
            ("get_float_type()", "float"), \
            ("get_double_type()", "double"), \
            ("get_long_double_type()", "longdouble") \
            ]

complex_types = [
        ("get_complex_type(get_float_type())", "cfloat"), \
        ("get_complex_type(get_double_type())", "cdouble"), \
        ("get_complex_type(get_long_double_type())", "clongdouble") \
        ]

integer_types = [
            ("get_signed_char_type()", "char"), \
            ("get_unsigned_char_type()", "uchar"), \
            ("get_signed_short_int_type()", "short"), \
            ("get_unsigned_short_int_type()", "ushort"), \
            ("get_signed_int_type()", "int"), \
            ("get_unsigned_int_type()", "uint"), \
            ("get_signed_long_int_type()", "long"), \
            ("get_unsigned_long_int_type()", "ulong"), \
            ("get_signed_long_long_int_type()", "longlong"), \
            ("get_unsigned_long_long_int_type()", "ulonglong"), \
            ("get_bool_type()", "_Bool")  \
            ]

all_types = integer_types + float_types +  complex_types

zero = lambda t : "0"
float_zero = lambda t : "0.0"
one = lambda t : "1"
float_one = lambda t : "1.0"
neg_zero = lambda t : "~0";

def get_identity(t, kind):
    if (t  == "int"):
        return "mercurium_int_%s" % (kind);
    elif (t  == "uint"):
        return "mercurium_uint_%s" % (kind);
    elif (t  == "char"):
        return "mercurium_schar_%s" % (kind);
    elif (t  == "uchar"):
        return "mercurium_uchar_%s" % (kind);
    elif (t  == "short"):
        return "mercurium_shrt_%s" % (kind);
    elif (t  == "ushort"):
        return "mercurium_ushrt_%s" % (kind);
    elif (t  == "long"):
        return "mercurium_long_%s" % (kind);
    elif (t  == "longlong"):
        return "mercurium_long_long_%s" % (kind);
    elif (t  == "ulong"):
        return "mercurium_ulong_%s" % (kind);
    elif (t  == "ulonglong"):
        return "mercurium_ulong_long_%s" % (kind);
    elif (t == "float"):
        return "mercurium_flt_%s" % (kind);
    elif (t == "double"):
        return "mercurium_dbl_%s" % (kind);
    elif (t == "longdouble"):
        return "mercurium_ldbl_%s" % (kind);
    elif (t == "_Bool"):
        if (kind == "max"):
            return "1";
        elif (kind == "min"):
            return "0";
        else:
            raise BaseException("Unhandled op %s for type %s" % (kind, t))
    else:
        raise BaseException("Unhandled type %s" % (t))

def max_identity(t):
    return get_identity(t, "min")

def min_identity(t):
    return get_identity(t, "max")

builtin_reductions = [ \
            # arithmetic operators
            ("+", "add", integer_types, zero), \
            ("+", "add", float_types, float_zero), \
            ("+", "add", complex_types, float_zero), \
            ("-", "sub", integer_types, zero), \
            ("-", "sub", float_types, float_zero), \
            ("-", "sub", complex_types, float_zero), \
            ("*", "prod", integer_types, one), \
            ("*", "prod", float_types, float_one), \
            ("*", "prod", complex_types, float_one), \
            # logic bit operators
            ("&", "and", integer_types, neg_zero), \
            ("|", "or", integer_types, zero), \
            ("^", "xor", integer_types, zero), \
            ("&&", "land", all_types, one), \
            ("||", "lor", all_types, zero), \
            # max
            ("max", "max", integer_types, max_identity), \
            ("max", "max", float_types, max_identity), \
            ("min", "min", integer_types, min_identity), \
            ("min", "min", float_types, min_identity), \
            ]

print "// DO NOT MODIFY THIS FILE"
print "// It will be overwritten when builtin-reductions.py changes"
print ""
print "#include \"tl-omp.hpp\""
print "#include \"tl-omp-udr.hpp\""
print "#include \"tl-type.hpp\""

print "void TL::OpenMP::initialize_builtin_udr_reductions(TL::Scope global_scope)"
print "{"
print "    static bool already_initialized = false;"
print "    if (already_initialized)"
print "        return;"
print "    already_initialized = true;"

for red in builtin_reductions:
    (op, internal_op_name, types, identity) = red
    for t in types :
        (constructor, internal_type_name) = t
        print "   {"
        print "      UDRInfoItem builtin_udr;"

        print "      type_t* t = %s;" % ( constructor )
        print "      builtin_udr.set_type(t);"

        print "      Source identity_src;"
        print "      identity_src << \"%s\";" % (identity(internal_type_name))
        print "      if (IS_FORTRAN_LANGUAGE)"
        print "         Source::source_language = SourceLanguage::C;"
        print "      Nodecl::NodeclBase identity = identity_src.parse_expression(global_scope);"
        print "      if (IS_FORTRAN_LANGUAGE)"
        print "         Source::source_language = SourceLanguage::Current;"
        print "      builtin_udr.set_identity(identity);"

        print "      builtin_udr.set_name(\"%s\");"  % (op)

        print "      // Query nanos++ function name"
        print "      TL::Symbol function = global_scope.get_symbol_from_name(\"nanos_reduction_bop_%s_%s\");" % (internal_op_name, internal_type_name)
        print "      ERROR_CONDITION(!function.is_valid(), \"Builtin function '%%s' not found\", \"nanos_reduction_bop_%s_%s\");" % (internal_op_name, internal_type_name)
        print "      builtin_udr.set_basic_reductor_function(function);"
        print "      // Cleanup function"
        print "      TL::Symbol cleanup_function = global_scope.get_symbol_from_name(\"nanos_reduction_default_cleanup_%s\");" % (internal_type_name)
        print "      ERROR_CONDITION(!cleanup_function.is_valid(), \"Builtin cleanup function '%%s' not found\", \"nanos_reduction_default_cleanup_%s\");" % (internal_type_name)
        print "      builtin_udr.set_cleanup_function(cleanup_function);"

        print "      builtin_udr.sign_in_scope(global_scope);"
        print "   }"

print "}"

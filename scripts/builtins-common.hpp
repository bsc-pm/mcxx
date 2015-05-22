#ifndef BUILTINS_COMMON_HPP
#define BUILTINS_COMMON_HPP

template <typename T>
struct generate_type
{
};

template <>
struct generate_type<void>
{
    static std::string g() { return "get_void_type()"; }
};

template <>
struct generate_type<int>
{
    static std::string g() { return "get_signed_int_type()"; }
};

template <>
struct generate_type<char>
{
    static std::string g() { return "get_char_type()"; }
};

template <>
struct generate_type<signed char>
{
    static std::string g() { return "get_signed_char_type()"; }
};

template <>
struct generate_type<short>
{
    static std::string g() { return "get_signed_short_int_type()"; }
};

template <>
struct generate_type<long>
{
    static std::string g() { return "get_signed_long_int_type()"; }
};

template <>
struct generate_type<long long>
{
    static std::string g() { return "get_signed_long_long_int_type()"; }
};

template <>
struct generate_type<unsigned int>
{
    static std::string g() { return "get_unsigned_int_type()"; }
};

template <>
struct generate_type<unsigned char>
{
    static std::string g() { return "get_unsigned_char_type()"; }
};

template <>
struct generate_type<unsigned short>
{
    static std::string g() { return "get_unsigned_short_int_type()"; }
};

template <>
struct generate_type<unsigned long>
{
    static std::string g() { return "get_unsigned_long_int_type()"; }
};

template <>
struct generate_type<unsigned long long>
{
    static std::string g() { return "get_unsigned_long_long_int_type()"; }
};

template <>
struct generate_type<float>
{
    static std::string g() { return "get_float_type()"; }
};

template <>
struct generate_type<double>
{
    static std::string g() { return "get_double_type()"; }
};

template <>
struct generate_type<long double>
{
    static std::string g() { return "get_long_double_type()"; }
};

#define GENERATE_VECTOR(N, T) \
template <>\
struct generate_type<__attribute__((vector_size(N))) T>\
{\
    static const int size = N;\
    typedef T element_type;\
\
    static std::string g() \
    {\
        std::stringstream ss;\
\
        ss << "get_vector_type(" << generate_type<element_type>::g() << ", " << N << ")";\
\
        return ss.str();\
    }\
};\

#define GENERATE_MANY(T) \
   GENERATE_VECTOR(8, T) \
   GENERATE_VECTOR(16, T) \
   GENERATE_VECTOR(32, T) \
   GENERATE_VECTOR(64, T)


GENERATE_MANY(int)
GENERATE_MANY(signed char)
GENERATE_MANY(char)
GENERATE_MANY(short)
GENERATE_MANY(long)
GENERATE_MANY(long long)

GENERATE_MANY(unsigned int)
GENERATE_MANY(unsigned char)
GENERATE_MANY(unsigned short)
GENERATE_MANY(unsigned long)
GENERATE_MANY(unsigned long long)

GENERATE_MANY(float)
GENERATE_MANY(double)

template <typename T>
struct generate_type<T*>
{
    static std::string g() 
    { 
        std::stringstream ss;

        ss << "get_pointer_type(" << generate_type<T>::g() << ")";
        return ss.str();
    }
};

template <typename T>
struct generate_type<const T*>
{
    static std::string g() 
    { 
        std::stringstream ss;

        ss << "get_pointer_type(get_const_qualified_type(" << generate_type<T>::g() << "))";
        return ss.str();
    }
};

template <typename T>
struct generate_type<volatile T*>
{
    static std::string g() 
    { 
        std::stringstream ss;

        ss << "get_pointer_type(get_volatile_qualified_type(" << generate_type<T>::g() << "))";
        return ss.str();
    }
};

template <typename T>
struct generate_type<const volatile T*>
{
    static std::string g() 
    { 
        std::stringstream ss;

        ss << "get_pointer_type(get_const_qualified_type(get_volatile_qualified_type(" << generate_type<T>::g() << ")))";
        return ss.str();
    }
};

template <typename R>
struct generate_type<R()>
{
    typedef R return_type;

    static std::string g() 
    {
        std::stringstream ss;

        ss 
            << "({"
            << "type_t* return_type = " << generate_type<return_type>::g() << ";\n"
            << "get_new_function_type(return_type, 0, 0, REF_QUALIFIER_NONE);\n"
            << "})\n"
            ;

        return ss.str();
    }
};

template <typename R, typename T1>
struct generate_type<R(T1)>
{
    typedef R return_type;
    typedef T1 param1_type;

    static std::string g() 
    {
        std::stringstream ss;

        ss 
            << "({"
            << "type_t* return_type = " << generate_type<return_type>::g() << ";\n"
            << "parameter_info_t p[1]; memset(p, 0, sizeof(p));"
            << "p[0].type_info = " << generate_type<param1_type>::g() << ";\n"
            << "get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);\n"
            << "})\n"
            ;
        return ss.str();
    }
};

template <typename R, typename T1, typename T2>
struct generate_type<R(T1, T2)>
{
    typedef R return_type;
    typedef T1 param1_type;
    typedef T2 param2_type;

    static std::string g() 
    {
        std::stringstream ss;

        ss 
            << "({"
            << "type_t* return_type = " << generate_type<return_type>::g() << ";\n"
            << "parameter_info_t p[2]; memset(p, 0, sizeof(p));"
            << "p[0].type_info = " << generate_type<param1_type>::g() << ";\n"
            << "p[1].type_info = " << generate_type<param2_type>::g() << ";\n"
            << "get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);\n"
            << "})\n"
            ;
        return ss.str();
    }
};

template <typename R, typename T1, typename T2, typename T3>
struct generate_type<R(T1, T2, T3)>
{
    typedef R return_type;
    typedef T1 param1_type;
    typedef T2 param2_type;
    typedef T3 param3_type;

    static std::string g() 
    {
        std::stringstream ss;
        ss 
            << "({"
            << "type_t* return_type = " << generate_type<return_type>::g() << ";\n"
            << "parameter_info_t p[3]; memset(p, 0, sizeof(p));"
            << "p[0].type_info = " << generate_type<param1_type>::g() << ";\n"
            << "p[1].type_info = " << generate_type<param2_type>::g() << ";\n"
            << "p[2].type_info = " << generate_type<param3_type>::g() << ";\n"
            << "get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);\n"
            << "})\n"
            ;
        return ss.str();
    }
};

template <typename R, typename T1, typename T2, typename T3, typename T4>
struct generate_type<R(T1, T2, T3, T4)>
{
    typedef R return_type;
    typedef T1 param1_type;
    typedef T2 param2_type;
    typedef T3 param3_type;
    typedef T4 param4_type;

    static std::string g() 
    {
        std::stringstream ss;
        ss
            << "({"
            << "type_t* return_type = " << generate_type<return_type>::g() << ";\n"
            << "parameter_info_t p[4]; memset(p, 0, sizeof(p));"
            << "p[0].type_info = " << generate_type<param1_type>::g() << ";\n"
            << "p[1].type_info = " << generate_type<param2_type>::g() << ";\n"
            << "p[2].type_info = " << generate_type<param3_type>::g() << ";\n"
            << "p[3].type_info = " << generate_type<param4_type>::g() << ";\n"
            << "get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);\n"
            << "})\n"
            ;
        return ss.str();
    }
};

template <typename R, typename T1, typename T2, typename T3, typename T4, typename T5>
struct generate_type<R(T1, T2, T3, T4, T5)>
{
    typedef R return_type;
    typedef T1 param1_type;
    typedef T2 param2_type;
    typedef T3 param3_type;
    typedef T4 param4_type;
    typedef T5 param5_type;

    static std::string g() 
    {
        std::stringstream ss;
        ss
            << "({"
            << "type_t* return_type = " << generate_type<return_type>::g() << ";\n"
            << "parameter_info_t p[5]; memset(p, 0, sizeof(p));"
            << "p[0].type_info = " << generate_type<param1_type>::g() << ";\n"
            << "p[1].type_info = " << generate_type<param2_type>::g() << ";\n"
            << "p[2].type_info = " << generate_type<param3_type>::g() << ";\n"
            << "p[3].type_info = " << generate_type<param4_type>::g() << ";\n"
            << "p[4].type_info = " << generate_type<param5_type>::g() << ";\n"
            << "get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);\n"
            << "})\n"
            ;
        return ss.str();
    }
};

template <typename R, typename T1, typename T2, typename T3, typename T4, typename T5, typename T6>
struct generate_type<R(T1, T2, T3, T4, T5, T6)>
{
    typedef R return_type;
    typedef T1 param1_type;
    typedef T2 param2_type;
    typedef T3 param3_type;
    typedef T4 param4_type;
    typedef T5 param5_type;
    typedef T6 param6_type;

    static std::string g() 
    {
        std::stringstream ss;
        ss
            << "({"
            << "type_t* return_type = " << generate_type<return_type>::g() << ";\n"
            << "parameter_info_t p[6]; memset(p, 0, sizeof(p));"
            << "p[0].type_info = " << generate_type<param1_type>::g() << ";\n"
            << "p[1].type_info = " << generate_type<param2_type>::g() << ";\n"
            << "p[2].type_info = " << generate_type<param3_type>::g() << ";\n"
            << "p[3].type_info = " << generate_type<param4_type>::g() << ";\n"
            << "p[4].type_info = " << generate_type<param5_type>::g() << ";\n"
            << "p[5].type_info = " << generate_type<param6_type>::g() << ";\n"
            << "get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);\n"
            << "})\n"
            ;
        return ss.str();
    }
};

template <typename R, typename T1, typename T2, typename T3, typename T4, typename T5, typename T6, typename T7>
struct generate_type<R(T1, T2, T3, T4, T5, T6, T7)>
{
    typedef R return_type;
    typedef T1 param1_type;
    typedef T2 param2_type;
    typedef T3 param3_type;
    typedef T4 param4_type;
    typedef T5 param5_type;
    typedef T6 param6_type;
    typedef T7 param7_type;

    static std::string g() 
    {
        std::stringstream ss;
        ss
            << "({"
            << "type_t* return_type = " << generate_type<return_type>::g() << ";\n"
            << "parameter_info_t p[7]; memset(p, 0, sizeof(p));"
            << "p[0].type_info = " << generate_type<param1_type>::g() << ";\n"
            << "p[1].type_info = " << generate_type<param2_type>::g() << ";\n"
            << "p[2].type_info = " << generate_type<param3_type>::g() << ";\n"
            << "p[3].type_info = " << generate_type<param4_type>::g() << ";\n"
            << "p[4].type_info = " << generate_type<param5_type>::g() << ";\n"
            << "p[5].type_info = " << generate_type<param6_type>::g() << ";\n"
            << "p[6].type_info = " << generate_type<param7_type>::g() << ";\n"
            << "get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);\n"
            << "})\n"
            ;
        return ss.str();
    }
};

template <typename R, typename T1, typename T2, typename T3, typename T4, typename T5, typename T6, typename T7, typename T8>
struct generate_type<R(T1, T2, T3, T4, T5, T6, T7, T8)>
{
    typedef R return_type;
    typedef T1 param1_type;
    typedef T2 param2_type;
    typedef T3 param3_type;
    typedef T4 param4_type;
    typedef T5 param5_type;
    typedef T6 param6_type;
    typedef T7 param7_type;
    typedef T8 param8_type;

    static std::string g() 
    {
        std::stringstream ss;
        ss
            << "({"
            << "type_t* return_type = " << generate_type<return_type>::g() << ";\n"
            << "parameter_info_t p[8]; memset(p, 0, sizeof(p));"
            << "p[0].type_info = " << generate_type<param1_type>::g() << ";\n"
            << "p[1].type_info = " << generate_type<param2_type>::g() << ";\n"
            << "p[2].type_info = " << generate_type<param3_type>::g() << ";\n"
            << "p[3].type_info = " << generate_type<param4_type>::g() << ";\n"
            << "p[4].type_info = " << generate_type<param5_type>::g() << ";\n"
            << "p[5].type_info = " << generate_type<param6_type>::g() << ";\n"
            << "p[6].type_info = " << generate_type<param7_type>::g() << ";\n"
            << "p[7].type_info = " << generate_type<param8_type>::g() << ";\n"
            << "get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);\n"
            << "})\n"
            ;
        return ss.str();
    }
};

template <typename T>
void f(const std::string& str)
{
    std::cout 
        << "{\n"
        << "scope_entry_t* sym_" << str << " = new_symbol(decl_context, decl_context->current_scope, uniquestr(\"" << str << "\"));\n"
        << "sym_" << str << "->kind = SK_FUNCTION;"
        << "sym_" << str << "->do_not_print = 1;\n"
        << "sym_" << str << "->type_information = " << generate_type<T>::g() << ";\n"
        << "symbol_entity_specs_set_is_builtin(sym_" << str << ", 1);\n"
        << "}\n"
        ;
}

#endif // BUILTINS_COMMON_HPP

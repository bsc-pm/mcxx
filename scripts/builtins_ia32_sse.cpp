//
// Generator of src/frontend/cxx-gccbuiltin-sse.h for gcc
//
// Compile it with g++-4.8 -fabi-version=6 -mavx -maes -mpclmul -mxop
// or use make generate_sse
//

#include <iostream> 
#include <sstream> 

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
   GENERATE_VECTOR(32, T)


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
        << "scope_entry_t* sym_" << str << " = new_symbol(decl_context, decl_context.current_scope, \"" << str << "\");\n"
        << "sym_" << str << "->kind = SK_FUNCTION;"
        << "sym_" << str << "->do_not_print = 1;\n"
        << "sym_" << str << "->type_information = " << generate_type<T>::g() << ";\n"
        << "sym_" << str << "->entity_specs.is_builtin = 1;\n"
        << "}\n"
        ;
}

#define END


#define VECTOR_INTRINSICS_LIST \
VECTOR_INTRIN(__builtin_ia32_addcarryx_u32) \
VECTOR_INTRIN(__builtin_ia32_addcarryx_u64) \
VECTOR_INTRIN(__builtin_ia32_addpd) \
VECTOR_INTRIN(__builtin_ia32_addpd256) \
VECTOR_INTRIN(__builtin_ia32_addps) \
VECTOR_INTRIN(__builtin_ia32_addps256) \
VECTOR_INTRIN(__builtin_ia32_addsd) \
VECTOR_INTRIN(__builtin_ia32_addss) \
VECTOR_INTRIN(__builtin_ia32_addsubpd) \
VECTOR_INTRIN(__builtin_ia32_addsubpd256) \
VECTOR_INTRIN(__builtin_ia32_addsubps) \
VECTOR_INTRIN(__builtin_ia32_addsubps256) \
VECTOR_INTRIN(__builtin_ia32_aesdec128) \
VECTOR_INTRIN(__builtin_ia32_aesdeclast128) \
VECTOR_INTRIN(__builtin_ia32_aesenc128) \
VECTOR_INTRIN(__builtin_ia32_aesenclast128) \
VECTOR_INTRIN(__builtin_ia32_aesimc128) \
VECTOR_INTRIN(__builtin_ia32_aeskeygenassist128) \
VECTOR_INTRIN(__builtin_ia32_andnpd) \
VECTOR_INTRIN(__builtin_ia32_andnpd256) \
VECTOR_INTRIN(__builtin_ia32_andnps) \
VECTOR_INTRIN(__builtin_ia32_andnps256) \
VECTOR_INTRIN(__builtin_ia32_andpd) \
VECTOR_INTRIN(__builtin_ia32_andpd256) \
VECTOR_INTRIN(__builtin_ia32_andps) \
VECTOR_INTRIN(__builtin_ia32_andps256) \
VECTOR_INTRIN(__builtin_ia32_blendpd) \
VECTOR_INTRIN(__builtin_ia32_blendpd256) \
VECTOR_INTRIN(__builtin_ia32_blendps) \
VECTOR_INTRIN(__builtin_ia32_blendps256) \
VECTOR_INTRIN(__builtin_ia32_blendvpd) \
VECTOR_INTRIN(__builtin_ia32_blendvpd256) \
VECTOR_INTRIN(__builtin_ia32_blendvps) \
VECTOR_INTRIN(__builtin_ia32_blendvps256) \
VECTOR_INTRIN(__builtin_ia32_bsrdi) \
VECTOR_INTRIN(__builtin_ia32_bsrsi) \
VECTOR_INTRIN(__builtin_ia32_clflush) \
VECTOR_INTRIN(__builtin_ia32_cmpeqpd) \
VECTOR_INTRIN(__builtin_ia32_cmpeqps) \
VECTOR_INTRIN(__builtin_ia32_cmpeqsd) \
VECTOR_INTRIN(__builtin_ia32_cmpeqss) \
VECTOR_INTRIN(__builtin_ia32_cmpgepd) \
VECTOR_INTRIN(__builtin_ia32_cmpgeps) \
VECTOR_INTRIN(__builtin_ia32_cmpgtpd) \
VECTOR_INTRIN(__builtin_ia32_cmpgtps) \
VECTOR_INTRIN(__builtin_ia32_cmplepd) \
VECTOR_INTRIN(__builtin_ia32_cmpleps) \
VECTOR_INTRIN(__builtin_ia32_cmplesd) \
VECTOR_INTRIN(__builtin_ia32_cmpless) \
VECTOR_INTRIN(__builtin_ia32_cmpltpd) \
VECTOR_INTRIN(__builtin_ia32_cmpltps) \
VECTOR_INTRIN(__builtin_ia32_cmpltsd) \
VECTOR_INTRIN(__builtin_ia32_cmpltss) \
VECTOR_INTRIN(__builtin_ia32_cmpneqpd) \
VECTOR_INTRIN(__builtin_ia32_cmpneqps) \
VECTOR_INTRIN(__builtin_ia32_cmpneqsd) \
VECTOR_INTRIN(__builtin_ia32_cmpneqss) \
VECTOR_INTRIN(__builtin_ia32_cmpngepd) \
VECTOR_INTRIN(__builtin_ia32_cmpngeps) \
VECTOR_INTRIN(__builtin_ia32_cmpngtpd) \
VECTOR_INTRIN(__builtin_ia32_cmpngtps) \
VECTOR_INTRIN(__builtin_ia32_cmpnlepd) \
VECTOR_INTRIN(__builtin_ia32_cmpnleps) \
VECTOR_INTRIN(__builtin_ia32_cmpnlesd) \
VECTOR_INTRIN(__builtin_ia32_cmpnless) \
VECTOR_INTRIN(__builtin_ia32_cmpnltpd) \
VECTOR_INTRIN(__builtin_ia32_cmpnltps) \
VECTOR_INTRIN(__builtin_ia32_cmpnltsd) \
VECTOR_INTRIN(__builtin_ia32_cmpnltss) \
VECTOR_INTRIN(__builtin_ia32_cmpordpd) \
VECTOR_INTRIN(__builtin_ia32_cmpordps) \
VECTOR_INTRIN(__builtin_ia32_cmpordsd) \
VECTOR_INTRIN(__builtin_ia32_cmpordss) \
VECTOR_INTRIN(__builtin_ia32_cmppd) \
VECTOR_INTRIN(__builtin_ia32_cmppd256) \
VECTOR_INTRIN(__builtin_ia32_cmpps) \
VECTOR_INTRIN(__builtin_ia32_cmpps256) \
VECTOR_INTRIN(__builtin_ia32_cmpsd) \
VECTOR_INTRIN(__builtin_ia32_cmpss) \
VECTOR_INTRIN(__builtin_ia32_cmpunordpd) \
VECTOR_INTRIN(__builtin_ia32_cmpunordps) \
VECTOR_INTRIN(__builtin_ia32_cmpunordsd) \
VECTOR_INTRIN(__builtin_ia32_cmpunordss) \
VECTOR_INTRIN(__builtin_ia32_comieq) \
VECTOR_INTRIN(__builtin_ia32_comige) \
VECTOR_INTRIN(__builtin_ia32_comigt) \
VECTOR_INTRIN(__builtin_ia32_comile) \
VECTOR_INTRIN(__builtin_ia32_comilt) \
VECTOR_INTRIN(__builtin_ia32_comineq) \
VECTOR_INTRIN(__builtin_ia32_comisdeq) \
VECTOR_INTRIN(__builtin_ia32_comisdge) \
VECTOR_INTRIN(__builtin_ia32_comisdgt) \
VECTOR_INTRIN(__builtin_ia32_comisdle) \
VECTOR_INTRIN(__builtin_ia32_comisdlt) \
VECTOR_INTRIN(__builtin_ia32_comisdneq) \
VECTOR_INTRIN(__builtin_ia32_crc32di) \
VECTOR_INTRIN(__builtin_ia32_crc32hi) \
VECTOR_INTRIN(__builtin_ia32_crc32qi) \
VECTOR_INTRIN(__builtin_ia32_crc32si) \
VECTOR_INTRIN(__builtin_ia32_cvtdq2pd) \
VECTOR_INTRIN(__builtin_ia32_cvtdq2pd256) \
VECTOR_INTRIN(__builtin_ia32_cvtdq2ps) \
VECTOR_INTRIN(__builtin_ia32_cvtdq2ps256) \
VECTOR_INTRIN(__builtin_ia32_cvtpd2dq) \
VECTOR_INTRIN(__builtin_ia32_cvtpd2dq256) \
VECTOR_INTRIN(__builtin_ia32_cvtpd2pi) \
VECTOR_INTRIN(__builtin_ia32_cvtpd2ps) \
VECTOR_INTRIN(__builtin_ia32_cvtpd2ps256) \
VECTOR_INTRIN(__builtin_ia32_cvtpi2pd) \
VECTOR_INTRIN(__builtin_ia32_cvtpi2ps) \
VECTOR_INTRIN(__builtin_ia32_cvtps2dq) \
VECTOR_INTRIN(__builtin_ia32_cvtps2dq256) \
VECTOR_INTRIN(__builtin_ia32_cvtps2pd) \
VECTOR_INTRIN(__builtin_ia32_cvtps2pd256) \
VECTOR_INTRIN(__builtin_ia32_cvtps2pi) \
VECTOR_INTRIN(__builtin_ia32_cvtsd2si) \
VECTOR_INTRIN(__builtin_ia32_cvtsd2si64) \
VECTOR_INTRIN(__builtin_ia32_cvtsd2ss) \
VECTOR_INTRIN(__builtin_ia32_cvtsi2sd) \
VECTOR_INTRIN(__builtin_ia32_cvtsi2ss) \
VECTOR_INTRIN(__builtin_ia32_cvtsi642sd) \
VECTOR_INTRIN(__builtin_ia32_cvtsi642ss) \
VECTOR_INTRIN(__builtin_ia32_cvtss2sd) \
VECTOR_INTRIN(__builtin_ia32_cvtss2si) \
VECTOR_INTRIN(__builtin_ia32_cvtss2si64) \
VECTOR_INTRIN(__builtin_ia32_cvttpd2dq) \
VECTOR_INTRIN(__builtin_ia32_cvttpd2dq256) \
VECTOR_INTRIN(__builtin_ia32_cvttpd2pi) \
VECTOR_INTRIN(__builtin_ia32_cvttps2dq) \
VECTOR_INTRIN(__builtin_ia32_cvttps2dq256) \
VECTOR_INTRIN(__builtin_ia32_cvttps2pi) \
VECTOR_INTRIN(__builtin_ia32_cvttsd2si) \
VECTOR_INTRIN(__builtin_ia32_cvttsd2si64) \
VECTOR_INTRIN(__builtin_ia32_cvttss2si) \
VECTOR_INTRIN(__builtin_ia32_cvttss2si64) \
VECTOR_INTRIN(__builtin_ia32_divpd) \
VECTOR_INTRIN(__builtin_ia32_divpd256) \
VECTOR_INTRIN(__builtin_ia32_divps) \
VECTOR_INTRIN(__builtin_ia32_divps256) \
VECTOR_INTRIN(__builtin_ia32_divsd) \
VECTOR_INTRIN(__builtin_ia32_divss) \
VECTOR_INTRIN(__builtin_ia32_dppd) \
VECTOR_INTRIN(__builtin_ia32_dpps) \
VECTOR_INTRIN(__builtin_ia32_dpps256) \
VECTOR_INTRIN(__builtin_ia32_emms) \
VECTOR_INTRIN(__builtin_ia32_pause) \
VECTOR_INTRIN(__builtin_ia32_fxrstor) \
VECTOR_INTRIN(__builtin_ia32_fxrstor64) \
VECTOR_INTRIN(__builtin_ia32_fxsave) \
VECTOR_INTRIN(__builtin_ia32_fxsave64) \
VECTOR_INTRIN(__builtin_ia32_haddpd) \
VECTOR_INTRIN(__builtin_ia32_haddpd256) \
VECTOR_INTRIN(__builtin_ia32_haddps) \
VECTOR_INTRIN(__builtin_ia32_haddps256) \
VECTOR_INTRIN(__builtin_ia32_hsubpd) \
VECTOR_INTRIN(__builtin_ia32_hsubpd256) \
VECTOR_INTRIN(__builtin_ia32_hsubps) \
VECTOR_INTRIN(__builtin_ia32_hsubps256) \
VECTOR_INTRIN(__builtin_ia32_insertps128) \
VECTOR_INTRIN(__builtin_ia32_lddqu) \
VECTOR_INTRIN(__builtin_ia32_lddqu256) \
VECTOR_INTRIN(__builtin_ia32_ldmxcsr) \
VECTOR_INTRIN(__builtin_ia32_lfence) \
VECTOR_INTRIN(__builtin_ia32_loaddqu) \
VECTOR_INTRIN(__builtin_ia32_loaddqu256) \
VECTOR_INTRIN(__builtin_ia32_loadhpd) \
VECTOR_INTRIN(__builtin_ia32_loadhps) \
VECTOR_INTRIN(__builtin_ia32_loadlpd) \
VECTOR_INTRIN(__builtin_ia32_loadlps) \
VECTOR_INTRIN(__builtin_ia32_loadupd) \
VECTOR_INTRIN(__builtin_ia32_loadupd256) \
VECTOR_INTRIN(__builtin_ia32_loadups) \
VECTOR_INTRIN(__builtin_ia32_loadups256) \
VECTOR_INTRIN(__builtin_ia32_maskloadpd) \
VECTOR_INTRIN(__builtin_ia32_maskloadpd256) \
VECTOR_INTRIN(__builtin_ia32_maskloadps) \
VECTOR_INTRIN(__builtin_ia32_maskloadps256) \
VECTOR_INTRIN(__builtin_ia32_maskmovdqu) \
VECTOR_INTRIN(__builtin_ia32_maskmovq) \
VECTOR_INTRIN(__builtin_ia32_maskstorepd) \
VECTOR_INTRIN(__builtin_ia32_maskstorepd256) \
VECTOR_INTRIN(__builtin_ia32_maskstoreps) \
VECTOR_INTRIN(__builtin_ia32_maskstoreps256) \
VECTOR_INTRIN(__builtin_ia32_maxpd) \
VECTOR_INTRIN(__builtin_ia32_maxpd256) \
VECTOR_INTRIN(__builtin_ia32_maxps) \
VECTOR_INTRIN(__builtin_ia32_maxps256) \
VECTOR_INTRIN(__builtin_ia32_maxsd) \
VECTOR_INTRIN(__builtin_ia32_maxss) \
VECTOR_INTRIN(__builtin_ia32_mfence) \
VECTOR_INTRIN(__builtin_ia32_minpd) \
VECTOR_INTRIN(__builtin_ia32_minpd256) \
VECTOR_INTRIN(__builtin_ia32_minps) \
VECTOR_INTRIN(__builtin_ia32_minps256) \
VECTOR_INTRIN(__builtin_ia32_minsd) \
VECTOR_INTRIN(__builtin_ia32_minss) \
VECTOR_INTRIN(__builtin_ia32_monitor) \
VECTOR_INTRIN(__builtin_ia32_movddup256) \
VECTOR_INTRIN(__builtin_ia32_movhlps) \
VECTOR_INTRIN(__builtin_ia32_movlhps) \
VECTOR_INTRIN(__builtin_ia32_movmskpd) \
VECTOR_INTRIN(__builtin_ia32_movmskpd256) \
VECTOR_INTRIN(__builtin_ia32_movmskps) \
VECTOR_INTRIN(__builtin_ia32_movmskps256) \
VECTOR_INTRIN(__builtin_ia32_movntdq) \
VECTOR_INTRIN(__builtin_ia32_movntdq256) \
VECTOR_INTRIN(__builtin_ia32_movntdqa) \
VECTOR_INTRIN(__builtin_ia32_movnti) \
VECTOR_INTRIN(__builtin_ia32_movnti64) \
VECTOR_INTRIN(__builtin_ia32_movntpd) \
VECTOR_INTRIN(__builtin_ia32_movntpd256) \
VECTOR_INTRIN(__builtin_ia32_movntps) \
VECTOR_INTRIN(__builtin_ia32_movntps256) \
VECTOR_INTRIN(__builtin_ia32_movntq) \
VECTOR_INTRIN(__builtin_ia32_movq128) \
VECTOR_INTRIN(__builtin_ia32_movsd) \
VECTOR_INTRIN(__builtin_ia32_movshdup) \
VECTOR_INTRIN(__builtin_ia32_movshdup256) \
VECTOR_INTRIN(__builtin_ia32_movsldup) \
VECTOR_INTRIN(__builtin_ia32_movsldup256) \
VECTOR_INTRIN(__builtin_ia32_movss) \
VECTOR_INTRIN(__builtin_ia32_mpsadbw128) \
VECTOR_INTRIN(__builtin_ia32_mulpd) \
VECTOR_INTRIN(__builtin_ia32_mulpd256) \
VECTOR_INTRIN(__builtin_ia32_mulps) \
VECTOR_INTRIN(__builtin_ia32_mulps256) \
VECTOR_INTRIN(__builtin_ia32_mulsd) \
VECTOR_INTRIN(__builtin_ia32_mulss) \
VECTOR_INTRIN(__builtin_ia32_mwait) \
VECTOR_INTRIN(__builtin_ia32_orpd) \
VECTOR_INTRIN(__builtin_ia32_orpd256) \
VECTOR_INTRIN(__builtin_ia32_orps) \
VECTOR_INTRIN(__builtin_ia32_orps256) \
VECTOR_INTRIN(__builtin_ia32_pabsb) \
VECTOR_INTRIN(__builtin_ia32_pabsb128) \
VECTOR_INTRIN(__builtin_ia32_pabsd) \
VECTOR_INTRIN(__builtin_ia32_pabsd128) \
VECTOR_INTRIN(__builtin_ia32_pabsw) \
VECTOR_INTRIN(__builtin_ia32_pabsw128) \
VECTOR_INTRIN(__builtin_ia32_packssdw) \
VECTOR_INTRIN(__builtin_ia32_packssdw128) \
VECTOR_INTRIN(__builtin_ia32_packsswb) \
VECTOR_INTRIN(__builtin_ia32_packsswb128) \
VECTOR_INTRIN(__builtin_ia32_packusdw128) \
VECTOR_INTRIN(__builtin_ia32_packuswb) \
VECTOR_INTRIN(__builtin_ia32_packuswb128) \
VECTOR_INTRIN(__builtin_ia32_paddb) \
VECTOR_INTRIN(__builtin_ia32_paddb128) \
VECTOR_INTRIN(__builtin_ia32_paddd) \
VECTOR_INTRIN(__builtin_ia32_paddd128) \
VECTOR_INTRIN(__builtin_ia32_paddq) \
VECTOR_INTRIN(__builtin_ia32_paddq128) \
VECTOR_INTRIN(__builtin_ia32_paddsb) \
VECTOR_INTRIN(__builtin_ia32_paddsb128) \
VECTOR_INTRIN(__builtin_ia32_paddsw) \
VECTOR_INTRIN(__builtin_ia32_paddsw128) \
VECTOR_INTRIN(__builtin_ia32_paddusb) \
VECTOR_INTRIN(__builtin_ia32_paddusb128) \
VECTOR_INTRIN(__builtin_ia32_paddusw) \
VECTOR_INTRIN(__builtin_ia32_paddusw128) \
VECTOR_INTRIN(__builtin_ia32_paddw) \
VECTOR_INTRIN(__builtin_ia32_paddw128) \
VECTOR_INTRIN(__builtin_ia32_palignr) \
VECTOR_INTRIN(__builtin_ia32_palignr128) \
VECTOR_INTRIN(__builtin_ia32_pand) \
VECTOR_INTRIN(__builtin_ia32_pand128) \
VECTOR_INTRIN(__builtin_ia32_pandn) \
VECTOR_INTRIN(__builtin_ia32_pandn128) \
VECTOR_INTRIN(__builtin_ia32_pavgb) \
VECTOR_INTRIN(__builtin_ia32_pavgb128) \
VECTOR_INTRIN(__builtin_ia32_pavgw) \
VECTOR_INTRIN(__builtin_ia32_pavgw128) \
VECTOR_INTRIN(__builtin_ia32_pblendvb128) \
VECTOR_INTRIN(__builtin_ia32_pblendw128) \
VECTOR_INTRIN(__builtin_ia32_pclmulqdq128) \
VECTOR_INTRIN(__builtin_ia32_pcmpeqb) \
VECTOR_INTRIN(__builtin_ia32_pcmpeqb128) \
VECTOR_INTRIN(__builtin_ia32_pcmpeqd) \
VECTOR_INTRIN(__builtin_ia32_pcmpeqd128) \
VECTOR_INTRIN(__builtin_ia32_pcmpeqq) \
VECTOR_INTRIN(__builtin_ia32_pcmpeqw) \
VECTOR_INTRIN(__builtin_ia32_pcmpeqw128) \
VECTOR_INTRIN(__builtin_ia32_pcmpestri128) \
VECTOR_INTRIN(__builtin_ia32_pcmpestria128) \
VECTOR_INTRIN(__builtin_ia32_pcmpestric128) \
VECTOR_INTRIN(__builtin_ia32_pcmpestrio128) \
VECTOR_INTRIN(__builtin_ia32_pcmpestris128) \
VECTOR_INTRIN(__builtin_ia32_pcmpestriz128) \
VECTOR_INTRIN(__builtin_ia32_pcmpestrm128) \
VECTOR_INTRIN(__builtin_ia32_pcmpgtb) \
VECTOR_INTRIN(__builtin_ia32_pcmpgtb128) \
VECTOR_INTRIN(__builtin_ia32_pcmpgtd) \
VECTOR_INTRIN(__builtin_ia32_pcmpgtd128) \
VECTOR_INTRIN(__builtin_ia32_pcmpgtq) \
VECTOR_INTRIN(__builtin_ia32_pcmpgtw) \
VECTOR_INTRIN(__builtin_ia32_pcmpgtw128) \
VECTOR_INTRIN(__builtin_ia32_pcmpistri128) \
VECTOR_INTRIN(__builtin_ia32_pcmpistria128) \
VECTOR_INTRIN(__builtin_ia32_pcmpistric128) \
VECTOR_INTRIN(__builtin_ia32_pcmpistrio128) \
VECTOR_INTRIN(__builtin_ia32_pcmpistris128) \
VECTOR_INTRIN(__builtin_ia32_pcmpistriz128) \
VECTOR_INTRIN(__builtin_ia32_pcmpistrm128) \
VECTOR_INTRIN(__builtin_ia32_pd256_pd) \
VECTOR_INTRIN(__builtin_ia32_pd_pd256) \
VECTOR_INTRIN(__builtin_ia32_phaddd) \
VECTOR_INTRIN(__builtin_ia32_phaddd128) \
VECTOR_INTRIN(__builtin_ia32_phaddsw) \
VECTOR_INTRIN(__builtin_ia32_phaddsw128) \
VECTOR_INTRIN(__builtin_ia32_phaddw) \
VECTOR_INTRIN(__builtin_ia32_phaddw128) \
VECTOR_INTRIN(__builtin_ia32_phminposuw128) \
VECTOR_INTRIN(__builtin_ia32_phsubd) \
VECTOR_INTRIN(__builtin_ia32_phsubd128) \
VECTOR_INTRIN(__builtin_ia32_phsubsw) \
VECTOR_INTRIN(__builtin_ia32_phsubsw128) \
VECTOR_INTRIN(__builtin_ia32_phsubw) \
VECTOR_INTRIN(__builtin_ia32_phsubw128) \
VECTOR_INTRIN(__builtin_ia32_pmaddubsw) \
VECTOR_INTRIN(__builtin_ia32_pmaddubsw128) \
VECTOR_INTRIN(__builtin_ia32_pmaddwd) \
VECTOR_INTRIN(__builtin_ia32_pmaddwd128) \
VECTOR_INTRIN(__builtin_ia32_pmaxsb128) \
VECTOR_INTRIN(__builtin_ia32_pmaxsd128) \
VECTOR_INTRIN(__builtin_ia32_pmaxsw) \
VECTOR_INTRIN(__builtin_ia32_pmaxsw128) \
VECTOR_INTRIN(__builtin_ia32_pmaxub) \
VECTOR_INTRIN(__builtin_ia32_pmaxub128) \
VECTOR_INTRIN(__builtin_ia32_pmaxud128) \
VECTOR_INTRIN(__builtin_ia32_pmaxuw128) \
VECTOR_INTRIN(__builtin_ia32_pminsb128) \
VECTOR_INTRIN(__builtin_ia32_pminsd128) \
VECTOR_INTRIN(__builtin_ia32_pminsw) \
VECTOR_INTRIN(__builtin_ia32_pminsw128) \
VECTOR_INTRIN(__builtin_ia32_pminub) \
VECTOR_INTRIN(__builtin_ia32_pminub128) \
VECTOR_INTRIN(__builtin_ia32_pminud128) \
VECTOR_INTRIN(__builtin_ia32_pminuw128) \
VECTOR_INTRIN(__builtin_ia32_pmovmskb) \
VECTOR_INTRIN(__builtin_ia32_pmovmskb128) \
VECTOR_INTRIN(__builtin_ia32_pmovsxbd128) \
VECTOR_INTRIN(__builtin_ia32_pmovsxbq128) \
VECTOR_INTRIN(__builtin_ia32_pmovsxbw128) \
VECTOR_INTRIN(__builtin_ia32_pmovsxdq128) \
VECTOR_INTRIN(__builtin_ia32_pmovsxwd128) \
VECTOR_INTRIN(__builtin_ia32_pmovsxwq128) \
VECTOR_INTRIN(__builtin_ia32_pmovzxbd128) \
VECTOR_INTRIN(__builtin_ia32_pmovzxbq128) \
VECTOR_INTRIN(__builtin_ia32_pmovzxbw128) \
VECTOR_INTRIN(__builtin_ia32_pmovzxdq128) \
VECTOR_INTRIN(__builtin_ia32_pmovzxwd128) \
VECTOR_INTRIN(__builtin_ia32_pmovzxwq128) \
VECTOR_INTRIN(__builtin_ia32_pmuldq128) \
VECTOR_INTRIN(__builtin_ia32_pmulhrsw) \
VECTOR_INTRIN(__builtin_ia32_pmulhrsw128) \
VECTOR_INTRIN(__builtin_ia32_pmulhuw) \
VECTOR_INTRIN(__builtin_ia32_pmulhuw128) \
VECTOR_INTRIN(__builtin_ia32_pmulhw) \
VECTOR_INTRIN(__builtin_ia32_pmulhw128) \
VECTOR_INTRIN(__builtin_ia32_pmulld128) \
VECTOR_INTRIN(__builtin_ia32_pmullw) \
VECTOR_INTRIN(__builtin_ia32_pmullw128) \
VECTOR_INTRIN(__builtin_ia32_pmuludq) \
VECTOR_INTRIN(__builtin_ia32_pmuludq128) \
VECTOR_INTRIN(__builtin_ia32_por) \
VECTOR_INTRIN(__builtin_ia32_por128) \
VECTOR_INTRIN(__builtin_ia32_ps256_ps) \
VECTOR_INTRIN(__builtin_ia32_psadbw) \
VECTOR_INTRIN(__builtin_ia32_psadbw128) \
VECTOR_INTRIN(__builtin_ia32_pshufb) \
VECTOR_INTRIN(__builtin_ia32_pshufb128) \
VECTOR_INTRIN(__builtin_ia32_pshufd) \
VECTOR_INTRIN(__builtin_ia32_pshufhw) \
VECTOR_INTRIN(__builtin_ia32_pshuflw) \
VECTOR_INTRIN(__builtin_ia32_pshufw) \
VECTOR_INTRIN(__builtin_ia32_psignb) \
VECTOR_INTRIN(__builtin_ia32_psignb128) \
VECTOR_INTRIN(__builtin_ia32_psignd) \
VECTOR_INTRIN(__builtin_ia32_psignd128) \
VECTOR_INTRIN(__builtin_ia32_psignw) \
VECTOR_INTRIN(__builtin_ia32_psignw128) \
VECTOR_INTRIN(__builtin_ia32_pslld) \
VECTOR_INTRIN(__builtin_ia32_pslld128) \
VECTOR_INTRIN(__builtin_ia32_pslldi) \
VECTOR_INTRIN(__builtin_ia32_pslldi128) \
VECTOR_INTRIN(__builtin_ia32_pslldqi128) \
VECTOR_INTRIN(__builtin_ia32_psllq) \
VECTOR_INTRIN(__builtin_ia32_psllq128) \
VECTOR_INTRIN(__builtin_ia32_psllqi) \
VECTOR_INTRIN(__builtin_ia32_psllqi128) \
VECTOR_INTRIN(__builtin_ia32_psllw) \
VECTOR_INTRIN(__builtin_ia32_psllw128) \
VECTOR_INTRIN(__builtin_ia32_psllwi) \
VECTOR_INTRIN(__builtin_ia32_psllwi128) \
VECTOR_INTRIN(__builtin_ia32_ps_ps256) \
VECTOR_INTRIN(__builtin_ia32_psrad) \
VECTOR_INTRIN(__builtin_ia32_psrad128) \
VECTOR_INTRIN(__builtin_ia32_psradi) \
VECTOR_INTRIN(__builtin_ia32_psradi128) \
VECTOR_INTRIN(__builtin_ia32_psraw) \
VECTOR_INTRIN(__builtin_ia32_psraw128) \
VECTOR_INTRIN(__builtin_ia32_psrawi) \
VECTOR_INTRIN(__builtin_ia32_psrawi128) \
VECTOR_INTRIN(__builtin_ia32_psrld) \
VECTOR_INTRIN(__builtin_ia32_psrld128) \
VECTOR_INTRIN(__builtin_ia32_psrldi) \
VECTOR_INTRIN(__builtin_ia32_psrldi128) \
VECTOR_INTRIN(__builtin_ia32_psrldqi128) \
VECTOR_INTRIN(__builtin_ia32_psrlq) \
VECTOR_INTRIN(__builtin_ia32_psrlq128) \
VECTOR_INTRIN(__builtin_ia32_psrlqi) \
VECTOR_INTRIN(__builtin_ia32_psrlqi128) \
VECTOR_INTRIN(__builtin_ia32_psrlw) \
VECTOR_INTRIN(__builtin_ia32_psrlw128) \
VECTOR_INTRIN(__builtin_ia32_psrlwi) \
VECTOR_INTRIN(__builtin_ia32_psrlwi128) \
VECTOR_INTRIN(__builtin_ia32_psubb) \
VECTOR_INTRIN(__builtin_ia32_psubb128) \
VECTOR_INTRIN(__builtin_ia32_psubd) \
VECTOR_INTRIN(__builtin_ia32_psubd128) \
VECTOR_INTRIN(__builtin_ia32_psubq) \
VECTOR_INTRIN(__builtin_ia32_psubq128) \
VECTOR_INTRIN(__builtin_ia32_psubsb) \
VECTOR_INTRIN(__builtin_ia32_psubsb128) \
VECTOR_INTRIN(__builtin_ia32_psubsw) \
VECTOR_INTRIN(__builtin_ia32_psubsw128) \
VECTOR_INTRIN(__builtin_ia32_psubusb) \
VECTOR_INTRIN(__builtin_ia32_psubusb128) \
VECTOR_INTRIN(__builtin_ia32_psubusw) \
VECTOR_INTRIN(__builtin_ia32_psubusw128) \
VECTOR_INTRIN(__builtin_ia32_psubw) \
VECTOR_INTRIN(__builtin_ia32_psubw128) \
VECTOR_INTRIN(__builtin_ia32_ptestc128) \
VECTOR_INTRIN(__builtin_ia32_ptestc256) \
VECTOR_INTRIN(__builtin_ia32_ptestnzc128) \
VECTOR_INTRIN(__builtin_ia32_ptestnzc256) \
VECTOR_INTRIN(__builtin_ia32_ptestz128) \
VECTOR_INTRIN(__builtin_ia32_ptestz256) \
VECTOR_INTRIN(__builtin_ia32_punpckhbw) \
VECTOR_INTRIN(__builtin_ia32_punpckhbw128) \
VECTOR_INTRIN(__builtin_ia32_punpckhdq) \
VECTOR_INTRIN(__builtin_ia32_punpckhdq128) \
VECTOR_INTRIN(__builtin_ia32_punpckhqdq128) \
VECTOR_INTRIN(__builtin_ia32_punpckhwd) \
VECTOR_INTRIN(__builtin_ia32_punpckhwd128) \
VECTOR_INTRIN(__builtin_ia32_punpcklbw) \
VECTOR_INTRIN(__builtin_ia32_punpcklbw128) \
VECTOR_INTRIN(__builtin_ia32_punpckldq) \
VECTOR_INTRIN(__builtin_ia32_punpckldq128) \
VECTOR_INTRIN(__builtin_ia32_punpcklqdq128) \
VECTOR_INTRIN(__builtin_ia32_punpcklwd) \
VECTOR_INTRIN(__builtin_ia32_punpcklwd128) \
VECTOR_INTRIN(__builtin_ia32_pxor) \
VECTOR_INTRIN(__builtin_ia32_pxor128) \
VECTOR_INTRIN(__builtin_ia32_rcpps) \
VECTOR_INTRIN(__builtin_ia32_rcpps256) \
VECTOR_INTRIN(__builtin_ia32_rcpss) \
VECTOR_INTRIN(__builtin_ia32_rdpmc) \
VECTOR_INTRIN(__builtin_ia32_rdtsc) \
VECTOR_INTRIN(__builtin_ia32_rdtscp) \
VECTOR_INTRIN(__builtin_ia32_rolhi) \
VECTOR_INTRIN(__builtin_ia32_rolqi) \
VECTOR_INTRIN(__builtin_ia32_rorhi) \
VECTOR_INTRIN(__builtin_ia32_rorqi) \
VECTOR_INTRIN(__builtin_ia32_roundpd) \
VECTOR_INTRIN(__builtin_ia32_roundpd256) \
VECTOR_INTRIN(__builtin_ia32_roundps) \
VECTOR_INTRIN(__builtin_ia32_roundps256) \
VECTOR_INTRIN(__builtin_ia32_roundsd) \
VECTOR_INTRIN(__builtin_ia32_roundss) \
VECTOR_INTRIN(__builtin_ia32_rsqrtps) \
VECTOR_INTRIN(__builtin_ia32_rsqrtps256) \
VECTOR_INTRIN(__builtin_ia32_rsqrtps_nr256) \
VECTOR_INTRIN(__builtin_ia32_rsqrtss) \
VECTOR_INTRIN(__builtin_ia32_sfence) \
VECTOR_INTRIN(__builtin_ia32_shufpd) \
VECTOR_INTRIN(__builtin_ia32_shufpd256) \
VECTOR_INTRIN(__builtin_ia32_shufps) \
VECTOR_INTRIN(__builtin_ia32_shufps256) \
VECTOR_INTRIN(__builtin_ia32_si256_si) \
VECTOR_INTRIN(__builtin_ia32_si_si256) \
VECTOR_INTRIN(__builtin_ia32_sqrtpd) \
VECTOR_INTRIN(__builtin_ia32_sqrtpd256) \
VECTOR_INTRIN(__builtin_ia32_sqrtps) \
VECTOR_INTRIN(__builtin_ia32_sqrtps256) \
VECTOR_INTRIN(__builtin_ia32_sqrtps_nr256) \
VECTOR_INTRIN(__builtin_ia32_sqrtsd) \
VECTOR_INTRIN(__builtin_ia32_sqrtss) \
VECTOR_INTRIN(__builtin_ia32_stmxcsr) \
VECTOR_INTRIN(__builtin_ia32_storedqu) \
VECTOR_INTRIN(__builtin_ia32_storedqu256) \
VECTOR_INTRIN(__builtin_ia32_storehps) \
VECTOR_INTRIN(__builtin_ia32_storelps) \
VECTOR_INTRIN(__builtin_ia32_storeupd) \
VECTOR_INTRIN(__builtin_ia32_storeupd256) \
VECTOR_INTRIN(__builtin_ia32_storeups) \
VECTOR_INTRIN(__builtin_ia32_storeups256) \
VECTOR_INTRIN(__builtin_ia32_subpd) \
VECTOR_INTRIN(__builtin_ia32_subpd256) \
VECTOR_INTRIN(__builtin_ia32_subps) \
VECTOR_INTRIN(__builtin_ia32_subps256) \
VECTOR_INTRIN(__builtin_ia32_subsd) \
VECTOR_INTRIN(__builtin_ia32_subss) \
VECTOR_INTRIN(__builtin_ia32_ucomieq) \
VECTOR_INTRIN(__builtin_ia32_ucomige) \
VECTOR_INTRIN(__builtin_ia32_ucomigt) \
VECTOR_INTRIN(__builtin_ia32_ucomile) \
VECTOR_INTRIN(__builtin_ia32_ucomilt) \
VECTOR_INTRIN(__builtin_ia32_ucomineq) \
VECTOR_INTRIN(__builtin_ia32_ucomisdeq) \
VECTOR_INTRIN(__builtin_ia32_ucomisdge) \
VECTOR_INTRIN(__builtin_ia32_ucomisdgt) \
VECTOR_INTRIN(__builtin_ia32_ucomisdle) \
VECTOR_INTRIN(__builtin_ia32_ucomisdlt) \
VECTOR_INTRIN(__builtin_ia32_ucomisdneq) \
VECTOR_INTRIN(__builtin_ia32_unpckhpd) \
VECTOR_INTRIN(__builtin_ia32_unpckhpd256) \
VECTOR_INTRIN(__builtin_ia32_unpckhps) \
VECTOR_INTRIN(__builtin_ia32_unpckhps256) \
VECTOR_INTRIN(__builtin_ia32_unpcklpd) \
VECTOR_INTRIN(__builtin_ia32_unpcklpd256) \
VECTOR_INTRIN(__builtin_ia32_unpcklps) \
VECTOR_INTRIN(__builtin_ia32_unpcklps256) \
VECTOR_INTRIN(__builtin_ia32_vbroadcastf128_pd256) \
VECTOR_INTRIN(__builtin_ia32_vbroadcastf128_ps256) \
VECTOR_INTRIN(__builtin_ia32_vbroadcastsd256) \
VECTOR_INTRIN(__builtin_ia32_vbroadcastss) \
VECTOR_INTRIN(__builtin_ia32_vbroadcastss256) \
VECTOR_INTRIN(__builtin_ia32_vec_ext_v16qi) \
VECTOR_INTRIN(__builtin_ia32_vec_ext_v2df) \
VECTOR_INTRIN(__builtin_ia32_vec_ext_v2di) \
VECTOR_INTRIN(__builtin_ia32_vec_ext_v2si) \
VECTOR_INTRIN(__builtin_ia32_vec_ext_v4hi) \
VECTOR_INTRIN(__builtin_ia32_vec_ext_v4sf) \
VECTOR_INTRIN(__builtin_ia32_vec_ext_v4si) \
VECTOR_INTRIN(__builtin_ia32_vec_ext_v8hi) \
VECTOR_INTRIN(__builtin_ia32_vec_init_v2si) \
VECTOR_INTRIN(__builtin_ia32_vec_init_v4hi) \
VECTOR_INTRIN(__builtin_ia32_vec_init_v8qi) \
VECTOR_INTRIN(__builtin_ia32_vec_set_v16qi) \
VECTOR_INTRIN(__builtin_ia32_vec_set_v2di) \
VECTOR_INTRIN(__builtin_ia32_vec_set_v4hi) \
VECTOR_INTRIN(__builtin_ia32_vec_set_v4si) \
VECTOR_INTRIN(__builtin_ia32_vec_set_v8hi) \
VECTOR_INTRIN(__builtin_ia32_vextractf128_pd256) \
VECTOR_INTRIN(__builtin_ia32_vextractf128_ps256) \
VECTOR_INTRIN(__builtin_ia32_vextractf128_si256) \
VECTOR_INTRIN(__builtin_ia32_vfrczpd) \
VECTOR_INTRIN(__builtin_ia32_vfrczpd256) \
VECTOR_INTRIN(__builtin_ia32_vfrczps) \
VECTOR_INTRIN(__builtin_ia32_vfrczps256) \
VECTOR_INTRIN(__builtin_ia32_vfrczsd) \
VECTOR_INTRIN(__builtin_ia32_vfrczss) \
VECTOR_INTRIN(__builtin_ia32_vinsertf128_pd256) \
VECTOR_INTRIN(__builtin_ia32_vinsertf128_ps256) \
VECTOR_INTRIN(__builtin_ia32_vinsertf128_si256) \
VECTOR_INTRIN(__builtin_ia32_vpcmov) \
VECTOR_INTRIN(__builtin_ia32_vpcmov_v16hi256) \
VECTOR_INTRIN(__builtin_ia32_vpcmov_v16qi) \
VECTOR_INTRIN(__builtin_ia32_vpcmov_v2df) \
VECTOR_INTRIN(__builtin_ia32_vpcmov_v2di) \
VECTOR_INTRIN(__builtin_ia32_vpcmov_v32qi256) \
VECTOR_INTRIN(__builtin_ia32_vpcmov_v4df256) \
VECTOR_INTRIN(__builtin_ia32_vpcmov_v4di256) \
VECTOR_INTRIN(__builtin_ia32_vpcmov_v4sf) \
VECTOR_INTRIN(__builtin_ia32_vpcmov_v4si) \
VECTOR_INTRIN(__builtin_ia32_vpcmov_v8hi) \
VECTOR_INTRIN(__builtin_ia32_vpcmov_v8sf256) \
VECTOR_INTRIN(__builtin_ia32_vpcmov_v8si256) \
VECTOR_INTRIN(__builtin_ia32_vpcomeqb) \
VECTOR_INTRIN(__builtin_ia32_vpcomeqd) \
VECTOR_INTRIN(__builtin_ia32_vpcomeqq) \
VECTOR_INTRIN(__builtin_ia32_vpcomequb) \
VECTOR_INTRIN(__builtin_ia32_vpcomequd) \
VECTOR_INTRIN(__builtin_ia32_vpcomequq) \
VECTOR_INTRIN(__builtin_ia32_vpcomequw) \
VECTOR_INTRIN(__builtin_ia32_vpcomeqw) \
VECTOR_INTRIN(__builtin_ia32_vpcomfalseb) \
VECTOR_INTRIN(__builtin_ia32_vpcomfalsed) \
VECTOR_INTRIN(__builtin_ia32_vpcomfalseq) \
VECTOR_INTRIN(__builtin_ia32_vpcomfalseub) \
VECTOR_INTRIN(__builtin_ia32_vpcomfalseud) \
VECTOR_INTRIN(__builtin_ia32_vpcomfalseuq) \
VECTOR_INTRIN(__builtin_ia32_vpcomfalseuw) \
VECTOR_INTRIN(__builtin_ia32_vpcomfalsew) \
VECTOR_INTRIN(__builtin_ia32_vpcomgeb) \
VECTOR_INTRIN(__builtin_ia32_vpcomged) \
VECTOR_INTRIN(__builtin_ia32_vpcomgeq) \
VECTOR_INTRIN(__builtin_ia32_vpcomgeub) \
VECTOR_INTRIN(__builtin_ia32_vpcomgeud) \
VECTOR_INTRIN(__builtin_ia32_vpcomgeuq) \
VECTOR_INTRIN(__builtin_ia32_vpcomgeuw) \
VECTOR_INTRIN(__builtin_ia32_vpcomgew) \
VECTOR_INTRIN(__builtin_ia32_vpcomgtb) \
VECTOR_INTRIN(__builtin_ia32_vpcomgtd) \
VECTOR_INTRIN(__builtin_ia32_vpcomgtq) \
VECTOR_INTRIN(__builtin_ia32_vpcomgtub) \
VECTOR_INTRIN(__builtin_ia32_vpcomgtud) \
VECTOR_INTRIN(__builtin_ia32_vpcomgtuq) \
VECTOR_INTRIN(__builtin_ia32_vpcomgtuw) \
VECTOR_INTRIN(__builtin_ia32_vpcomgtw) \
VECTOR_INTRIN(__builtin_ia32_vpcomleb) \
VECTOR_INTRIN(__builtin_ia32_vpcomled) \
VECTOR_INTRIN(__builtin_ia32_vpcomleq) \
VECTOR_INTRIN(__builtin_ia32_vpcomleub) \
VECTOR_INTRIN(__builtin_ia32_vpcomleud) \
VECTOR_INTRIN(__builtin_ia32_vpcomleuq) \
VECTOR_INTRIN(__builtin_ia32_vpcomleuw) \
VECTOR_INTRIN(__builtin_ia32_vpcomlew) \
VECTOR_INTRIN(__builtin_ia32_vpcomltb) \
VECTOR_INTRIN(__builtin_ia32_vpcomltd) \
VECTOR_INTRIN(__builtin_ia32_vpcomltq) \
VECTOR_INTRIN(__builtin_ia32_vpcomltub) \
VECTOR_INTRIN(__builtin_ia32_vpcomltud) \
VECTOR_INTRIN(__builtin_ia32_vpcomltuq) \
VECTOR_INTRIN(__builtin_ia32_vpcomltuw) \
VECTOR_INTRIN(__builtin_ia32_vpcomltw) \
VECTOR_INTRIN(__builtin_ia32_vpcomneb) \
VECTOR_INTRIN(__builtin_ia32_vpcomned) \
VECTOR_INTRIN(__builtin_ia32_vpcomneq) \
VECTOR_INTRIN(__builtin_ia32_vpcomneub) \
VECTOR_INTRIN(__builtin_ia32_vpcomneud) \
VECTOR_INTRIN(__builtin_ia32_vpcomneuq) \
VECTOR_INTRIN(__builtin_ia32_vpcomneuw) \
VECTOR_INTRIN(__builtin_ia32_vpcomnew) \
VECTOR_INTRIN(__builtin_ia32_vpcomtrueb) \
VECTOR_INTRIN(__builtin_ia32_vpcomtrued) \
VECTOR_INTRIN(__builtin_ia32_vpcomtrueq) \
VECTOR_INTRIN(__builtin_ia32_vpcomtrueub) \
VECTOR_INTRIN(__builtin_ia32_vpcomtrueud) \
VECTOR_INTRIN(__builtin_ia32_vpcomtrueuq) \
VECTOR_INTRIN(__builtin_ia32_vpcomtrueuw) \
VECTOR_INTRIN(__builtin_ia32_vpcomtruew) \
VECTOR_INTRIN(__builtin_ia32_vperm2f128_pd256) \
VECTOR_INTRIN(__builtin_ia32_vperm2f128_ps256) \
VECTOR_INTRIN(__builtin_ia32_vperm2f128_si256) \
VECTOR_INTRIN(__builtin_ia32_vpermil2pd) \
VECTOR_INTRIN(__builtin_ia32_vpermil2pd256) \
VECTOR_INTRIN(__builtin_ia32_vpermil2ps) \
VECTOR_INTRIN(__builtin_ia32_vpermil2ps256) \
VECTOR_INTRIN(__builtin_ia32_vpermilpd) \
VECTOR_INTRIN(__builtin_ia32_vpermilpd256) \
VECTOR_INTRIN(__builtin_ia32_vpermilps) \
VECTOR_INTRIN(__builtin_ia32_vpermilps256) \
VECTOR_INTRIN(__builtin_ia32_vpermilvarpd) \
VECTOR_INTRIN(__builtin_ia32_vpermilvarpd256) \
VECTOR_INTRIN(__builtin_ia32_vpermilvarps) \
VECTOR_INTRIN(__builtin_ia32_vpermilvarps256) \
VECTOR_INTRIN(__builtin_ia32_vphaddbd) \
VECTOR_INTRIN(__builtin_ia32_vphaddbq) \
VECTOR_INTRIN(__builtin_ia32_vphaddbw) \
VECTOR_INTRIN(__builtin_ia32_vphadddq) \
VECTOR_INTRIN(__builtin_ia32_vphaddubd) \
VECTOR_INTRIN(__builtin_ia32_vphaddubq) \
VECTOR_INTRIN(__builtin_ia32_vphaddubw) \
VECTOR_INTRIN(__builtin_ia32_vphaddudq) \
VECTOR_INTRIN(__builtin_ia32_vphadduwd) \
VECTOR_INTRIN(__builtin_ia32_vphadduwq) \
VECTOR_INTRIN(__builtin_ia32_vphaddwd) \
VECTOR_INTRIN(__builtin_ia32_vphaddwq) \
VECTOR_INTRIN(__builtin_ia32_vphsubbw) \
VECTOR_INTRIN(__builtin_ia32_vphsubdq) \
VECTOR_INTRIN(__builtin_ia32_vphsubwd) \
VECTOR_INTRIN(__builtin_ia32_vpmacsdd) \
VECTOR_INTRIN(__builtin_ia32_vpmacsdqh) \
VECTOR_INTRIN(__builtin_ia32_vpmacsdql) \
VECTOR_INTRIN(__builtin_ia32_vpmacssdd) \
VECTOR_INTRIN(__builtin_ia32_vpmacssdqh) \
VECTOR_INTRIN(__builtin_ia32_vpmacssdql) \
VECTOR_INTRIN(__builtin_ia32_vpmacsswd) \
VECTOR_INTRIN(__builtin_ia32_vpmacssww) \
VECTOR_INTRIN(__builtin_ia32_vpmacswd) \
VECTOR_INTRIN(__builtin_ia32_vpmacsww) \
VECTOR_INTRIN(__builtin_ia32_vpmadcsswd) \
VECTOR_INTRIN(__builtin_ia32_vpmadcswd) \
VECTOR_INTRIN(__builtin_ia32_vpperm) \
VECTOR_INTRIN(__builtin_ia32_vprotb) \
VECTOR_INTRIN(__builtin_ia32_vprotd) \
VECTOR_INTRIN(__builtin_ia32_vprotq) \
VECTOR_INTRIN(__builtin_ia32_vprotw) \
VECTOR_INTRIN(__builtin_ia32_vpshab) \
VECTOR_INTRIN(__builtin_ia32_vpshad) \
VECTOR_INTRIN(__builtin_ia32_vpshaq) \
VECTOR_INTRIN(__builtin_ia32_vpshaw) \
VECTOR_INTRIN(__builtin_ia32_vpshlb) \
VECTOR_INTRIN(__builtin_ia32_vpshld) \
VECTOR_INTRIN(__builtin_ia32_vpshlq) \
VECTOR_INTRIN(__builtin_ia32_vpshlw) \
VECTOR_INTRIN(__builtin_ia32_vtestcpd) \
VECTOR_INTRIN(__builtin_ia32_vtestcpd256) \
VECTOR_INTRIN(__builtin_ia32_vtestcps) \
VECTOR_INTRIN(__builtin_ia32_vtestcps256) \
VECTOR_INTRIN(__builtin_ia32_vtestnzcpd) \
VECTOR_INTRIN(__builtin_ia32_vtestnzcpd256) \
VECTOR_INTRIN(__builtin_ia32_vtestnzcps) \
VECTOR_INTRIN(__builtin_ia32_vtestnzcps256) \
VECTOR_INTRIN(__builtin_ia32_vtestzpd) \
VECTOR_INTRIN(__builtin_ia32_vtestzpd256) \
VECTOR_INTRIN(__builtin_ia32_vtestzps) \
VECTOR_INTRIN(__builtin_ia32_vtestzps256) \
VECTOR_INTRIN(__builtin_ia32_vzeroall) \
VECTOR_INTRIN(__builtin_ia32_vzeroupper) \
VECTOR_INTRIN(__builtin_ia32_xorpd) \
VECTOR_INTRIN(__builtin_ia32_xorpd256) \
VECTOR_INTRIN(__builtin_ia32_xorps) \
VECTOR_INTRIN(__builtin_ia32_xorps256) \
END


int main(int, char**)
{
#define VECTOR_INTRIN(X) \
    f<__typeof__(X)>(#X);
    VECTOR_INTRINSICS_LIST
#undef VECTOR_INTRIN
}

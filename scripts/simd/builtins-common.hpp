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

// FIXME: Consider to pass this to C++11 eventually
template <typename R, 
         typename T1,
         typename T2,
         typename T3,
         typename T4,
         typename T5,
         typename T6,
         typename T7,
         typename T8,
         typename T9,
         typename T10,
         typename T11,
         typename T12,
         typename T13,
         typename T14,
         typename T15,
         typename T16
         >
struct generate_type<R(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)>
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
    typedef T9 param9_type;
    typedef T10 param10_type;
    typedef T11 param11_type;
    typedef T12 param12_type;
    typedef T13 param13_type;
    typedef T14 param14_type;
    typedef T15 param15_type;
    typedef T16 param16_type;

    static std::string g() 
    {
        std::stringstream ss;
        ss
            << "({"
            << "type_t* return_type = " << generate_type<return_type>::g() << ";\n"
            << "parameter_info_t p[16]; memset(p, 0, sizeof(p));"
            << "p[0].type_info = " << generate_type<param1_type>::g() << ";\n"
            << "p[1].type_info = " << generate_type<param2_type>::g() << ";\n"
            << "p[2].type_info = " << generate_type<param3_type>::g() << ";\n"
            << "p[3].type_info = " << generate_type<param4_type>::g() << ";\n"
            << "p[4].type_info = " << generate_type<param5_type>::g() << ";\n"
            << "p[5].type_info = " << generate_type<param6_type>::g() << ";\n"
            << "p[6].type_info = " << generate_type<param7_type>::g() << ";\n"
            << "p[7].type_info = " << generate_type<param8_type>::g() << ";\n"
            << "p[8].type_info = " << generate_type<param9_type>::g() << ";\n"
            << "p[9].type_info = " << generate_type<param10_type>::g() << ";\n"
            << "p[10].type_info = " << generate_type<param11_type>::g() << ";\n"
            << "p[11].type_info = " << generate_type<param12_type>::g() << ";\n"
            << "p[12].type_info = " << generate_type<param13_type>::g() << ";\n"
            << "p[13].type_info = " << generate_type<param14_type>::g() << ";\n"
            << "p[14].type_info = " << generate_type<param15_type>::g() << ";\n"
            << "p[15].type_info = " << generate_type<param16_type>::g() << ";\n"
            << "get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);\n"
            << "})\n"
            ;
        return ss.str();
    }
};

#define param_type(N) typedef T##N param##N##_type

template <typename R, 
         typename T1,
         typename T2,
         typename T3,
         typename T4,
         typename T5,
         typename T6,
         typename T7,
         typename T8,
         typename T9,
         typename T10,
         typename T11,
         typename T12,
         typename T13,
         typename T14,
         typename T15,
         typename T16,
         typename T17,
         typename T18,
         typename T19,
         typename T20,
         typename T21,
         typename T22,
         typename T23,
         typename T24,
         typename T25,
         typename T26,
         typename T27,
         typename T28,
         typename T29,
         typename T30,
         typename T31,
         typename T32
         >
struct generate_type<R(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13,
        T14, T15, T16, T17, T18, T19, T20, T21, T22, T23, T24, T25, T26, T27,
        T28, T29, T30, T31, T32)>
{
    typedef R return_type;
    param_type(1);
    param_type(2);
    param_type(3);
    param_type(4);
    param_type(5);
    param_type(6);
    param_type(7);
    param_type(8);
    param_type(9);
    param_type(10);
    param_type(11);
    param_type(12);
    param_type(13);
    param_type(14);
    param_type(15);
    param_type(16);
    param_type(17);
    param_type(18);
    param_type(19);
    param_type(20);
    param_type(21);
    param_type(22);
    param_type(23);
    param_type(24);
    param_type(25);
    param_type(26);
    param_type(27);
    param_type(28);
    param_type(29);
    param_type(30);
    param_type(31);
    param_type(32);

    static std::string g() 
    {
        std::stringstream ss;
        ss
            << "({"
            << "type_t* return_type = " << generate_type<return_type>::g() << ";\n"
            << "parameter_info_t p[32]; memset(p, 0, sizeof(p));"
            << "p[0].type_info = " << generate_type<param1_type>::g() << ";\n"
            << "p[1].type_info = " << generate_type<param2_type>::g() << ";\n"
            << "p[2].type_info = " << generate_type<param3_type>::g() << ";\n"
            << "p[3].type_info = " << generate_type<param4_type>::g() << ";\n"
            << "p[4].type_info = " << generate_type<param5_type>::g() << ";\n"
            << "p[5].type_info = " << generate_type<param6_type>::g() << ";\n"
            << "p[6].type_info = " << generate_type<param7_type>::g() << ";\n"
            << "p[7].type_info = " << generate_type<param8_type>::g() << ";\n"
            << "p[8].type_info = " << generate_type<param9_type>::g() << ";\n"
            << "p[9].type_info = " << generate_type<param10_type>::g() << ";\n"
            << "p[10].type_info = " << generate_type<param11_type>::g() << ";\n"
            << "p[11].type_info = " << generate_type<param12_type>::g() << ";\n"
            << "p[12].type_info = " << generate_type<param13_type>::g() << ";\n"
            << "p[13].type_info = " << generate_type<param14_type>::g() << ";\n"
            << "p[14].type_info = " << generate_type<param15_type>::g() << ";\n"
            << "p[15].type_info = " << generate_type<param16_type>::g() << ";\n"
            << "p[15].type_info = " << generate_type<param16_type>::g() << ";\n"
            << "p[16].type_info = " << generate_type<param17_type>::g() << ";\n"
            << "p[17].type_info = " << generate_type<param18_type>::g() << ";\n"
            << "p[18].type_info = " << generate_type<param19_type>::g() << ";\n"
            << "p[19].type_info = " << generate_type<param20_type>::g() << ";\n"
            << "p[20].type_info = " << generate_type<param21_type>::g() << ";\n"
            << "p[21].type_info = " << generate_type<param22_type>::g() << ";\n"
            << "p[22].type_info = " << generate_type<param23_type>::g() << ";\n"
            << "p[23].type_info = " << generate_type<param24_type>::g() << ";\n"
            << "p[24].type_info = " << generate_type<param25_type>::g() << ";\n"
            << "p[25].type_info = " << generate_type<param26_type>::g() << ";\n"
            << "p[26].type_info = " << generate_type<param27_type>::g() << ";\n"
            << "p[27].type_info = " << generate_type<param28_type>::g() << ";\n"
            << "p[28].type_info = " << generate_type<param29_type>::g() << ";\n"
            << "p[29].type_info = " << generate_type<param30_type>::g() << ";\n"
            << "p[30].type_info = " << generate_type<param31_type>::g() << ";\n"
            << "p[31].type_info = " << generate_type<param32_type>::g() << ";\n"
            << "get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);\n"
            << "})\n"
            ;
        return ss.str();
    }
};

template <typename R, 
         typename T1,
         typename T2,
         typename T3,
         typename T4,
         typename T5,
         typename T6,
         typename T7,
         typename T8,
         typename T9,
         typename T10,
         typename T11,
         typename T12,
         typename T13,
         typename T14,
         typename T15,
         typename T16,
         typename T17,
         typename T18,
         typename T19,
         typename T20,
         typename T21,
         typename T22,
         typename T23,
         typename T24,
         typename T25,
         typename T26,
         typename T27,
         typename T28,
         typename T29,
         typename T30,
         typename T31,
         typename T32,
         typename T33,
         typename T34,
         typename T35,
         typename T36,
         typename T37,
         typename T38,
         typename T39,
         typename T40,
         typename T41,
         typename T42,
         typename T43,
         typename T44,
         typename T45,
         typename T46,
         typename T47,
         typename T48,
         typename T49,
         typename T50,
         typename T51,
         typename T52,
         typename T53,
         typename T54,
         typename T55,
         typename T56,
         typename T57,
         typename T58,
         typename T59,
         typename T60,
         typename T61,
         typename T62,
         typename T63,
         typename T64
         >
struct generate_type<R(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13,
        T14, T15, T16, T17, T18, T19, T20, T21, T22, T23, T24, T25, T26, T27,
        T28, T29, T30, T31, T32, T33, T34, T35, T36, T37, T38, T39, T40, T41,
        T42, T43, T44, T45, T46, T47, T48, T49, T50, T51, T52, T53, T54, T55,
        T56, T57, T58, T59, T60, T61, T62, T63, T64
        )>
{
    typedef R return_type;
    param_type(1);
    param_type(2);
    param_type(3);
    param_type(4);
    param_type(5);
    param_type(6);
    param_type(7);
    param_type(8);
    param_type(9);
    param_type(10);
    param_type(11);
    param_type(12);
    param_type(13);
    param_type(14);
    param_type(15);
    param_type(16);
    param_type(17);
    param_type(18);
    param_type(19);
    param_type(20);
    param_type(21);
    param_type(22);
    param_type(23);
    param_type(24);
    param_type(25);
    param_type(26);
    param_type(27);
    param_type(28);
    param_type(29);
    param_type(30);
    param_type(31);
    param_type(32);
    param_type(33);
    param_type(34);
    param_type(35);
    param_type(36);
    param_type(37);
    param_type(38);
    param_type(39);
    param_type(40);
    param_type(41);
    param_type(42);
    param_type(43);
    param_type(44);
    param_type(45);
    param_type(46);
    param_type(47);
    param_type(48);
    param_type(49);
    param_type(50);
    param_type(51);
    param_type(52);
    param_type(53);
    param_type(54);
    param_type(55);
    param_type(56);
    param_type(57);
    param_type(58);
    param_type(59);
    param_type(60);
    param_type(61);
    param_type(62);
    param_type(63);
    param_type(64);

    static std::string g() 
    {
        std::stringstream ss;
        ss
            << "({"
            << "type_t* return_type = " << generate_type<return_type>::g() << ";\n"
            << "parameter_info_t p[64]; memset(p, 0, sizeof(p));"
            << "p[0].type_info = " << generate_type<param1_type>::g() << ";\n"
            << "p[1].type_info = " << generate_type<param2_type>::g() << ";\n"
            << "p[2].type_info = " << generate_type<param3_type>::g() << ";\n"
            << "p[3].type_info = " << generate_type<param4_type>::g() << ";\n"
            << "p[4].type_info = " << generate_type<param5_type>::g() << ";\n"
            << "p[5].type_info = " << generate_type<param6_type>::g() << ";\n"
            << "p[6].type_info = " << generate_type<param7_type>::g() << ";\n"
            << "p[7].type_info = " << generate_type<param8_type>::g() << ";\n"
            << "p[8].type_info = " << generate_type<param9_type>::g() << ";\n"
            << "p[9].type_info = " << generate_type<param10_type>::g() << ";\n"
            << "p[10].type_info = " << generate_type<param11_type>::g() << ";\n"
            << "p[11].type_info = " << generate_type<param12_type>::g() << ";\n"
            << "p[12].type_info = " << generate_type<param13_type>::g() << ";\n"
            << "p[13].type_info = " << generate_type<param14_type>::g() << ";\n"
            << "p[14].type_info = " << generate_type<param15_type>::g() << ";\n"
            << "p[15].type_info = " << generate_type<param16_type>::g() << ";\n"
            << "p[16].type_info = " << generate_type<param17_type>::g() << ";\n"
            << "p[17].type_info = " << generate_type<param18_type>::g() << ";\n"
            << "p[18].type_info = " << generate_type<param19_type>::g() << ";\n"
            << "p[19].type_info = " << generate_type<param20_type>::g() << ";\n"
            << "p[20].type_info = " << generate_type<param21_type>::g() << ";\n"
            << "p[21].type_info = " << generate_type<param22_type>::g() << ";\n"
            << "p[22].type_info = " << generate_type<param23_type>::g() << ";\n"
            << "p[23].type_info = " << generate_type<param24_type>::g() << ";\n"
            << "p[24].type_info = " << generate_type<param25_type>::g() << ";\n"
            << "p[25].type_info = " << generate_type<param26_type>::g() << ";\n"
            << "p[26].type_info = " << generate_type<param27_type>::g() << ";\n"
            << "p[27].type_info = " << generate_type<param28_type>::g() << ";\n"
            << "p[28].type_info = " << generate_type<param29_type>::g() << ";\n"
            << "p[29].type_info = " << generate_type<param30_type>::g() << ";\n"
            << "p[30].type_info = " << generate_type<param31_type>::g() << ";\n"
            << "p[31].type_info = " << generate_type<param32_type>::g() << ";\n"
            << "p[32].type_info = " << generate_type<param33_type>::g() << ";\n"
            << "p[33].type_info = " << generate_type<param34_type>::g() << ";\n"
            << "p[34].type_info = " << generate_type<param35_type>::g() << ";\n"
            << "p[35].type_info = " << generate_type<param36_type>::g() << ";\n"
            << "p[36].type_info = " << generate_type<param37_type>::g() << ";\n"
            << "p[37].type_info = " << generate_type<param38_type>::g() << ";\n"
            << "p[38].type_info = " << generate_type<param39_type>::g() << ";\n"
            << "p[39].type_info = " << generate_type<param40_type>::g() << ";\n"
            << "p[40].type_info = " << generate_type<param41_type>::g() << ";\n"
            << "p[41].type_info = " << generate_type<param42_type>::g() << ";\n"
            << "p[42].type_info = " << generate_type<param43_type>::g() << ";\n"
            << "p[43].type_info = " << generate_type<param44_type>::g() << ";\n"
            << "p[44].type_info = " << generate_type<param45_type>::g() << ";\n"
            << "p[45].type_info = " << generate_type<param46_type>::g() << ";\n"
            << "p[46].type_info = " << generate_type<param47_type>::g() << ";\n"
            << "p[47].type_info = " << generate_type<param48_type>::g() << ";\n"
            << "p[48].type_info = " << generate_type<param49_type>::g() << ";\n"
            << "p[49].type_info = " << generate_type<param50_type>::g() << ";\n"
            << "p[50].type_info = " << generate_type<param51_type>::g() << ";\n"
            << "p[51].type_info = " << generate_type<param52_type>::g() << ";\n"
            << "p[52].type_info = " << generate_type<param53_type>::g() << ";\n"
            << "p[53].type_info = " << generate_type<param54_type>::g() << ";\n"
            << "p[54].type_info = " << generate_type<param55_type>::g() << ";\n"
            << "p[55].type_info = " << generate_type<param56_type>::g() << ";\n"
            << "p[56].type_info = " << generate_type<param57_type>::g() << ";\n"
            << "p[57].type_info = " << generate_type<param58_type>::g() << ";\n"
            << "p[58].type_info = " << generate_type<param59_type>::g() << ";\n"
            << "p[59].type_info = " << generate_type<param60_type>::g() << ";\n"
            << "p[60].type_info = " << generate_type<param61_type>::g() << ";\n"
            << "p[61].type_info = " << generate_type<param62_type>::g() << ";\n"
            << "p[62].type_info = " << generate_type<param63_type>::g() << ";\n"
            << "p[63].type_info = " << generate_type<param64_type>::g() << ";\n"
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
        << "sym_" << str << "->locus = builtins_locus;\n"
        << "sym_" << str << "->type_information = " << generate_type<T>::g() << ";\n"
        << "symbol_entity_specs_set_is_builtin(sym_" << str << ", 1);\n"
        << "}\n"
        ;
}

#endif // BUILTINS_COMMON_HPP

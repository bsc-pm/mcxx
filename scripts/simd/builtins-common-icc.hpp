#ifndef BUILTINS_COMMON_ICC_HPP
#define BUILTINS_COMMON_ICC_HPP

// --------------------------------------------
// Specific generators for ICC SIMD
// --------------------------------------------

#define GENERATE_VECTOR(name) \
template <>\
struct generate_type<__##name>\
{\
    static std::string g() \
    {\
        return "get_user_defined_type(get_" #name "_typedef())";\
    }\
};

GENERATE_VECTOR(m64)

GENERATE_VECTOR(m128)
GENERATE_VECTOR(m128i)
GENERATE_VECTOR(m128d)

GENERATE_VECTOR(m256)
GENERATE_VECTOR(m256i)
GENERATE_VECTOR(m256d)

GENERATE_VECTOR(m512)
GENERATE_VECTOR(m512i)
GENERATE_VECTOR(m512d)

struct generate_type__int64
{
    static std::string g()
    {
        return "(type_get_size(get_signed_long_int_type()) == 8 ? get_signed_long_int_type() : get_signed_long_long_int_type())";
    }
};

struct generate_type__uint64
{
    static std::string g()
    {
        return "(type_get_size(get_unsigned_long_int_type()) == 8 ? get_unsigned_long_int_type() : get_unsigned_long_long_int_type())";
    }
};

// HACK HACK HACK
// Welcome to the hell of typesystems.
// You are not expected to understand this
// _bswap64 and others have a (unsigned) long long that is not the same long long of the C++ typesystem

template <typename T>
struct Extract1;
template <typename R, typename T>
struct Extract1<R(T)> { typedef T type; };
template <typename P> struct Extract1<P*> : Extract1<P> { };

template <>
struct generate_type<Extract1<decltype(_bswap64)>::type > : generate_type__int64 { };

template <typename T>
struct Extract0;
template <typename R, typename T>
struct Extract0<R(T)> { typedef R type; }; 
template <typename P>
struct Extract0<P*> : Extract0<P> { };

template <>
struct generate_type<Extract1<decltype(_castu64_f64)>::type > : generate_type__uint64 { };
// End of HACK HACK HACK

#endif // BUILTINS_COMMON_ICC_HPP

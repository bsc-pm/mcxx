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
        return "get_" #name "_struct_type()";\
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
struct generate_type<Extract1<decltype(_bswap64)>::type > : generate_type<long long> { };

template <typename T>
struct Extract0;
template <typename R, typename T>
struct Extract0<R(T)> { typedef R type; }; 
template <typename P>
struct Extract0<P*> : Extract0<P> { };

template <>
struct generate_type<Extract1<decltype(_castu64_f64)>::type > : generate_type<unsigned long long> { };
// End of HACK HACK HACK

#endif // BUILTINS_COMMON_ICC_HPP

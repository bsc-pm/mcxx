/*
<testinfo>
test_generator=config/mercurium
compile_versions="def nodef"
test_CFLAGS_def="-DDEFINE_FLOATS"
test_noexec=yes
</testinfo>
*/

// Only GCC 7 knows these types already so filter this case
#if defined(DEFINE_FLOATS) && __GNUC__ < 7
typedef float _Float32;
typedef float _Float32x;
typedef double _Float64;
typedef double _Float64x;
#if defined(__x86_64__) && defined(__GNUC__) \
    && (__GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 6))
typedef __float128 _Float128;
typedef __float128 _Float128x;
#endif
#endif

void test1(_Float32 a);
void test2(_Float32x a);
void test3(_Float64 a);
void test4(_Float64x a);
#if defined(__x86_64__) && defined(__GNUC__) \
    && (__GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 6))
void test5(_Float128 a);
void test6(_Float128x a);
#endif

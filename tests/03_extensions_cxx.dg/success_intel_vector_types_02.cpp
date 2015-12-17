/*
<testinfo>
test_generator=config/mercurium-extensions
test_CXXFLAGS="--env=linux-x86_64 --enable-ms-builtins --enable-intel-vector-types"
</testinfo>
*/

typedef struct __declspec(align(16)) __declspec(intrin_type) __m128 {
    float               m128_f32[4];
} __m128;

typedef struct __declspec(align(16)) __declspec(intrin_type) __m128d {
    double              m128d_f64[2];
} __m128d;
typedef union  __declspec(align(16)) __declspec(intrin_type) __m128i {
     __int64             m128i_gcc_compatibility[2];
    __int8              m128i_i8[16];
    __int16             m128i_i16[8];
    __int32             m128i_i32[4];
    __int64             m128i_i64[2];
    unsigned __int8     m128i_u8[16];
    unsigned __int16    m128i_u16[8];
    unsigned __int32    m128i_u32[4];
    unsigned __int64    m128i_u64[2];
    char c[16];
} __m128i;
typedef union  __declspec(align(32)) __declspec(intrin_type) __m256 {
    float m256_f32[8];
} __m256;
typedef struct __declspec(align(32)) __declspec(intrin_type) __m256d {
    double m256d_f64[4];
} __m256d;
typedef union  __declspec(align(32)) __declspec(intrin_type) __m256i {
    __int64             m256i_gcc_compatibility[4];
    __int8              m256i_i8[32];
    __int16             m256i_i16[16];
    __int32             m256i_i32[8];
    __int64             m256i_i64[4];
    unsigned __int8     m256i_u8[32];
    unsigned __int16    m256i_u16[16];
    unsigned __int32    m256i_u32[8];
    unsigned __int64    m256i_u64[4];
} __m256i;
typedef union __declspec(align(64)) __declspec(intrin_type) __m512 {
    float       __m512_f32[16];
} __m512;
typedef union __declspec(align(64)) __declspec(intrin_type) __m512d {
    double      __m512d_f64[8];
} __m512d;
typedef union __declspec(align(64)) __declspec(intrin_type) __m512i {
    int         __m512i_i32[16];
} __m512i;
void f(void)
{
    {
        __m128 m = { };
        __m128i mi = { };
        __m128d md = { };
        m = (__m128)mi;
        mi = (__m128i)m;
        m = (__m128)md;
        md = (__m128d)m;
        mi = (__m128i)md;
        md = (__m128d)mi;
    }
    {
        __m256 m = { };
        __m256i mi = { };
        __m256d md = { };
        m = (__m256)mi;
        mi = (__m256i)m;
        m = (__m256)md;
        md = (__m256d)m;
        mi = (__m256i)md;
        md = (__m256d)mi;
    }
    {
        __m512 m = { };
        __m512i mi = { };
        __m512d md = { };
        m = (__m512)mi;
        mi = (__m512i)m;
        m = (__m512)md;
        md = (__m512d)m;
        mi = (__m512i)md;
        md = (__m512d)mi;
    }
}

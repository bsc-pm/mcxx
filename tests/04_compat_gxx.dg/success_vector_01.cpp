/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

typedef float __m128 __attribute__((vector_size(16), __may_alias__));
typedef float __v4sf __attribute__((vector_size(16)));

struct A { int x; int y; };

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_set_ss (float __F)
{
    return __extension__ (__m128)(__v4sf){ __F, 0.0f, 0.0f, 0.0f };
}

/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

typedef float __m256 __attribute__ ((__vector_size__ (32), __may_alias__));

extern __inline __m256 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_setzero_ps (void)
{
  return __extension__ (__m256){ 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
}

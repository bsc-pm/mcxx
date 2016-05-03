/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/
#include<stdint.h>

template < typename T>
struct vec1
{
    union  { int a, b, c, d; };
};

typedef vec1<int32_t> vec1_i32_4;
static_assert(sizeof(vec1_i32_4) == 4, "incorrect sizeof(vec1_i32_4)");

template < typename T>
struct vec2
{
    union { T a, b; };
    union { T c, d; };
    union { T e, f; };
    union { T g, h; };
};


typedef vec2<int32_t> vec2_i32_4;
static_assert(sizeof(vec2_i32_4) == 16, "incorrect sizeof(vec2_i32_4)");

/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

#include<stdint.h>

struct X1
{
    uint32_t a, b, c;
};

static_assert(sizeof(X1) == 12, "incorrect sizeof(X1)");

struct X2
{

    uint32_t a, b, c;
    X1 d;
};

static_assert(sizeof(X2) == 24, "incorrect sizeof(X2)");


template < typename T, int n>
struct vec
{
    T a[n];
};


typedef vec<int32_t, 4> vec_i32_4;

static_assert(sizeof(vec_i32_4) == 16, "incorrect sizeof(vec_i32_4)");


struct X3
{
    vec_i32_4 a;
    int32_t b;
    char v[4];
};

static_assert(sizeof(X3) == 24, "incorrect sizeof(X3)");

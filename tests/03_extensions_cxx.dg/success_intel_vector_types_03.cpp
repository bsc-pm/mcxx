/*
<testinfo>
test_generator=config/mercurium-extensions
test_CXXFLAGS="--env=linux-x86_64 --enable-ms-builtins --enable-intel-vector-types"
</testinfo>
*/

struct __m512
{
    float a[16];
};

void g()
{
    __attribute__((vector_size(64))) float v1;
    __m512 v2;

    v1 = v1;
    v2 = v2;

    v1 = v2;
    v2 = v1;

}

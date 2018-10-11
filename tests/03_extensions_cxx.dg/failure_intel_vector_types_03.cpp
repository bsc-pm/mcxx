/*
<testinfo>
test_generator=config/mercurium-extensions
test_compile_fail=yes
test_CXXFLAGS="--env=linux-x86_64 --enable-ms-builtins --enable-intel-vector-types"
test_compile_end_signal=yes
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

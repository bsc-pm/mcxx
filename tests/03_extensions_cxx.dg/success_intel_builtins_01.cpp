/*
<testinfo>
test_generator=config/mercurium-extensions
test_CXXFLAGS="--enable-intel-builtins-syntax"
</testinfo>
*/

void f(int *x)
{
    __assume_aligned(x, 16);
}

/*
<testinfo>
test_generator=config/mercurium-extensions
test_CXXFLAGS=--enable-intel-builtins-syntax
</testinfo>
*/

void f(int x, int *y)
{
    __assume(x > 3);
    __assume_aligned(y, 16);
}

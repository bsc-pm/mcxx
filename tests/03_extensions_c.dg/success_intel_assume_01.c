/*
<testinfo>
test_generator=config/mercurium-extensions
test_CFLAGS=--enable-intel-builtins-syntax
</testinfo>
*/

void f(int x, int *y)
{
    __assume(x > 3);
    __assume_aligned(y, 16);
}

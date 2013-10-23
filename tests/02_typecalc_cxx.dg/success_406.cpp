/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

enum E { n };

template <typename T>
T g(T);

int operator+(void* (*)(void*), E);

void f(void)
{
    // We try here to fool the typechecker
    g + n<3>(4);
}

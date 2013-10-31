/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

enum E { n };

template <typename T>
T g(T);

int operator+(void* (*)(void*), E);
int operator+(E, void* (*)(void*));

void f(void)
{
    // OK
    g + n<3>(4);

    // OK
    (n + g)<3>(4);

    // OK
    n + (g)<3>(4);
}

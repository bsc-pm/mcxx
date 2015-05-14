/*
<testinfo>
test_generator=config/mercurium-fe-only
test_compile_fail=yes
</testinfo>
*/

enum E { n };

template <typename T>
T g(T);

int operator+(E, void* (*)(void*));

void f(void)
{
    // This is an error
    n + g<3>(4);
}


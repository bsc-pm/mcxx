/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
#include <cstddef>

template <typename T> struct Test { };
template <typename T> void* operator new(size_t, Test<T>* lazy);

struct Bar
{
    Bar(int);
};

void foo()
{
    Bar *b = new Bar(1);
    delete b;

    Test<int> *p;
    Bar *b1 = new (p) Bar(1);
}

/*
<testinfo>
test_generator="config/mercurium"
</testinfo>
*/

template <typename S>
static void foo(S);

template <typename T>
struct A
{
    void (*p)(int), (*q)(int);

    A() : p(&foo<int>), q(foo<int>) { }

};

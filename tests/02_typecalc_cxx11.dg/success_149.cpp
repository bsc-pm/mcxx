/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <typename T, typename ...S>
static void foo(T, S...);

struct A
{
    void (*p)(int, float, char);
    void (*q)(float, double);
    void (*s)(long);

    A() : p(&foo<int, float, char>),
          q(&foo<float, double>),
          s(&foo<long>) { }

};

struct B
{
    void (*p)(int, float, char);
    void (*q)(float, double);
    void (*s)(long);

    B() : p(foo<int, float, char>),
          q(foo<float, double>),
          s(foo<long>) { }

};

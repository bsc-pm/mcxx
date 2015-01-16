/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

template <typename T>
void g();

template <typename T>
void f(T t)
{
    auto x = t.foo();

    g<decltype(x)>();
}

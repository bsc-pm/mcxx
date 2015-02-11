/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

template <typename T>
struct A
{
};

template <typename T>
void h(T t);

template <typename T>
struct B : A<T>
{
    void f(T t);
    void g(T s)
    {
        h(s);
        f(s);
        auto m = [&](T t) { f(t); };
        m(T ());
    }
};

void h()
{
    B<int> b;

    b.g(1);
}

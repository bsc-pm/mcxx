/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T>
struct A
{
    A();
    operator T*();
    operator const T*() const;

};

template <typename T>
struct B
{
};

template <>
struct B<int>
{
    void one();
};

template <>
struct B<const int>
{
    void two();
};

template <typename T>
B<T> g(T&);

void f()
{

    A<int> a;
    a[2];
    g(a[2]).one();

    const A<int> b;
    b[2];
    g(b[2]).two();
}

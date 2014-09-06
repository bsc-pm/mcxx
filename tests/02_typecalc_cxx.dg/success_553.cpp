/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T, typename K>
int foo(const T& t)
{
    return t.x;
}

template <typename T, typename K, typename S, typename I>
int foo(const T& t)
{
    return t.x;
}

template <typename S, typename K, typename I>
struct A
{
    void bar()
    {
        foo<A, S, K, I>(*this);
    }

    friend int foo<A, S, K, I>(const A&);

    private:
    int x;

};


void g()
{
    A<int, float, double> a;
    a.bar();

    A<double, float, int> b;
    b.bar();
}

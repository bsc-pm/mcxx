/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T, typename K, typename S, typename I>
int foo(const T& t)
{
    return t.x;
}

template <typename T>
struct A
{
    typedef typename T::S S;
    typedef typename T::K K;
    typedef typename T::I I;

    void bar();

    friend int foo<A, S, K, I>(const A&); // S, K, I not found

    private:
    int x;

};

struct B
{
    typedef int S;
    typedef float K;
    typedef double I;
};

void g()
{
    A<B> a;

    a.bar();
}

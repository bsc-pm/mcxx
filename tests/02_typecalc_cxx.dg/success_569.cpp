/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
template <typename T, int N>
struct A
{
    template <int M>
    void b(int ) { }

    T x;
};

template <int N>
void f(int c)
{
    A<int, N> a;
    a.x<N>(c);
    a.template b<N>(c);
}

void g(int x)
{
    f<42>(x);
}

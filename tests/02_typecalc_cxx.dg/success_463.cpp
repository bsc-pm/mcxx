/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T>
struct A
{
    A();
    template <typename S, typename Q>
        A(const A<S>&, const A<Q>&);
};

template <typename T>
void f(const A<T> &a)
{
    const A<int> b = a + 1;

    A<double> c;

    A<float>(b, c);
}

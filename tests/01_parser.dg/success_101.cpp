namespace A
{
    template <typename _T>
    struct B
    {
        enum { value = 3 };
    };

    template <typename _T,
             int _N = B<_T>::value>
    struct C
    {
    };
}

namespace D
{
    template <typename _T>
    void g(A::C<_T> c1);

    void f(void)
    {
        A::C<float> c;

        g(c);
    }
}

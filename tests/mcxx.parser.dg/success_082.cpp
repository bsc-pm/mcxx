template <typename _T>
struct A
{
};

template <typename _Q>
struct B
{
};

template <typename _M>
struct C
{
    typedef typename _M::template T< A <_M> >::C N;

    typedef A<_M> A;

    void f(N) { }

    void g(N);
};

template <typename _T>
void C<_T>::g(typename C<_T>::N)
{
}

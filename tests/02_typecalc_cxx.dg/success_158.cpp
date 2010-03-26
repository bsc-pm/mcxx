template <typename _M>
struct C
{
    typedef _M quux;
};

struct B
{
    template <typename _M>
        struct foo
        {
            typedef _M baz;
        };
};


template <typename _T, typename _S>
struct A
{
    typedef typename _T::template foo<_S>::baz moo;

    typedef typename moo::quux bar;
};

typedef float *P;
typedef A<B, C<float*> >::bar P;

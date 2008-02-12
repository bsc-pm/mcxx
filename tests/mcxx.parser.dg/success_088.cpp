template <typename _T>
struct B
{
    typedef _T *T;
};

template <typename _T>
struct A
{
};

template <template <typename _Q> class _V>
struct A<_V<int> >
{
    typedef typename _V<float>::T T;
};

typedef float *p_float;
typedef A<B<int> >::T p_float;

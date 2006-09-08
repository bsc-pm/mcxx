template <class A>
struct B
{
    typedef A *T;
};

template <template <typename Q> class V,
         template <typename Q> class W = V>
struct N
{
    typedef typename W<int>::T K;
};

N<B>::K p;

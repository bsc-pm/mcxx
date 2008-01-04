template <typename _T>
struct A
{
    template <typename _S>
        struct B
        {
            typedef typename _T::K S;
        };

    typedef _T T;
};

struct C
{
    typedef float K;
};

A<C>::T k1;
A<C>::B<int>::S k2;

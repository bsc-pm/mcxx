template <typename _T0>
struct B
{
};

template <typename _T1>
struct C
{
};

template <typename _T2>
struct C<B<_T2> >
{
    typedef _T2 T;
};

C<
B<int>
>::T i;

C<
B<float>
>::T f;

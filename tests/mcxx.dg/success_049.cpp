template <typename _T, typename _Q>
struct A
{
};

template <typename _T>
struct B
{
};

template <typename _T2>
struct 
B<
A<_T2, _T2> 
>
{
};

template <typename _T1, typename _Q1>
struct B<A<_T1*, _Q1*> >
{
};


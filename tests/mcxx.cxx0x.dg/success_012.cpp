template <typename _T>
struct A
{
};

template <typename _T>
struct B
{
};

template <typename _T = A<B<int>>>
struct C
{
};

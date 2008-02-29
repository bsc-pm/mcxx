template <typename _T>
struct A
{
};

template <typename _T>
struct B
{
};

typedef A<B<int> > S;
typedef A<B<int>> S;

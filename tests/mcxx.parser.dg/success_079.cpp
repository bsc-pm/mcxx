template <typename _T>
struct A
{
};

template <typename _T>
struct B
{
    typedef A<_T> K;

    K k;
};

B<int>::K k;

/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <typename T>
struct A
{
};

template <typename T>
struct A<T*>
{
    typedef T Type;
};

template <typename S>
using B = A<S*>;

template <typename Q>
struct C : B<Q>
{
};

typedef C<int>::Type T1;
typedef int T1;

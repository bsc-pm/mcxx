/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <typename T1, typename T2>
struct A
{
    typedef T1 Type1;
    typedef T2 Type2;
};

template <typename T>
struct B
{
    template <typename S>
    using Foo = A<T*, S*>;
};

typedef B<double>::Foo<int>::Type1 T1;
typedef double* T1;

typedef B<double>::Foo<int>::Type2 T2;
typedef int* T2;

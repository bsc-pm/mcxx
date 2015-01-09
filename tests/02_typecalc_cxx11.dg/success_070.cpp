/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

typedef void * M;

template <typename T>
struct B
{
    typedef T* K;
};

template <typename T>
using A = typename B<T>::K;

template <typename T, typename S>
struct SameType
{
};

template <typename T>
struct SameType<T, T>
{
    typedef T Equal;
};

void g()
{
    A<void> b;

    decltype(b) c0;
    void* c1;

    SameType<decltype(c0), decltype(c1)>::Equal e;
}

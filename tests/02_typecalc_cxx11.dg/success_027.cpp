/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <typename ...T>
struct A
{
    const static int x = sizeof...(T);
};

template <int N, int M>
struct B
{
};

template <int C>
struct B<C, C>
{
    typedef int check;
};

void f()
{
    B<A<int, float>::x, 2>::check c;
    c = 1;

}

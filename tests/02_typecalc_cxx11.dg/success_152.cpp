/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

template <typename T>
struct A
{
};

template <template <typename T> class ...W>
void f(W<int>& ...w)
{
}

void g()
{
    A<int> a;

    f(a, a);
}

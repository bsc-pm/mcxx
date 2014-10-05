/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

template <typename T>
struct A
{
    A(int);
};

template <typename T>
using A_lias = A<T>;

template <typename T>
struct B : A_lias<T>
{
    B() : A_lias<T>(3) { }
};


template <typename T>
void f(A_lias<T>& a);

void g()
{
    A_lias<int> a(3);
    f(a);
}

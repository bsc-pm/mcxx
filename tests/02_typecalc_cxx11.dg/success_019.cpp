/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <typename T>
struct A { };

template <typename ...T>
struct B { };

template <typename ...T>
void f(A<T...> &a, B<T...> &b);

void g()
{
    A<int> a;
    B<int> b;

    ::f(a, b);
}

/*
<testinfo>
# This test uses mercurium-extensions because g++ 4.8 does not accept this code
test_generator="config/mercurium-extensions"
test_CXXFLAGS="-std=c++11"
</testinfo>
*/

template <typename R1, typename R2>
struct W1 { };

template <template <typename, typename> class W>
struct A { };

template <template <typename, typename> class ...W>
struct B { };

template <template <typename, typename> class ...W>
void f(A<W...> &a, B<W...> &b);

void g()
{
    A<W1> a;
    B<W1> b;

    ::f(a, b);
}

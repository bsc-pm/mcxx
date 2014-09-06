/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <typename R1, typename R2>
struct W1 { };

template <template <typename, typename> class W>
struct A { };

template <template <typename, typename> class ...W>
struct B { };

template <template <typename, typename> class ...W>
void f(A<W...> &a)
{
    a = 3;
}

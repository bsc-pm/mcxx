/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T>
struct A { };

template <typename T>
void f(A<T>& p)
{
    static_cast<A<T> >(p);
}

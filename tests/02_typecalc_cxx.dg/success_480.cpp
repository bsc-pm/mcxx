/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

enum E { a = 0, b = 1, c = 2, d = 3 };

template <E e>
struct A { };

void f(A<c>& c_);

void g()
{
    A<a> a_;
    A<b> b_;

    A<E(2)> c_;

    f(c_);
}

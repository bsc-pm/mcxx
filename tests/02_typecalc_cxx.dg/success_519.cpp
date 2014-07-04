/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

// Note the changing class/struct declarations
class A;
struct A { static int x; };

template <typename T> class B;
template <typename T> struct B { static int x; };

template <typename T> struct C;
template <typename T> class C { static int x; };
template <typename T> struct C<T*> { static int x; };

void f()
{
    A::x = 3;
    B<int>::x = 4;
    C<int*>::x = 3;
}

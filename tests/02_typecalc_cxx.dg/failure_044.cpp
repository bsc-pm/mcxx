/*
<testinfo>
test_generator=config/mercurium
test_compile_fail=yes
</testinfo>
*/

// Note the changing class/struct declarations
template <typename T> struct C;
template <typename T> class C { static int x; };
template <typename T> struct C<T*> { static int x; };

void f()
{
    C<int>::x = 3;
}

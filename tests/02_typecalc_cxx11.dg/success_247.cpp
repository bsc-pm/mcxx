/*
<testinfo>
test_generator="config/mercurium-cxx11"
test_nolink=yes
</testinfo>
*/

template <typename T1, int ...T2>
struct A { };

template <int ...T2>
using B = A<int, T2...>;

template <typename S, int ...Q> 
void foo(S, B<Q...>);

void bar()
{
    foo(1.0f, A<int, 10, 20>());
}

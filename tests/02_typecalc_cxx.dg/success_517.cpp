/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <int N, typename T>
struct B { };

template <int N, typename T>
struct B<N, T*> : B<N+1, T> { };

template <typename T>
struct D : B<0, T> { };

template <int N, typename T>
void f(B<N, T>&) { }

void t()
{
    D<int**> d;

    f<0>(d);
    f<1>(d);
}

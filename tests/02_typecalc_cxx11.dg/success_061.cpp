/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

struct A
{
    int x;
    constexpr A(int n) : x(n) { }
    constexpr int f() { return 2 * x; }
};

struct B : A
{
    int y;
    constexpr B(int n) : A(n), y(n + 1) { }
    constexpr int g() { return this->y + this->x; }
    constexpr int h() { return this->f() + 10; }
};

template <int N, int M>
struct C;

template <int N>
struct C<N, N>
{
    typedef int Type;
};

void f()
{
    C<B(25).f(), 50>::Type t1;
    C<B(25).g(), 51>::Type t2;
    C<B(25).h(), 60>::Type t3;
}

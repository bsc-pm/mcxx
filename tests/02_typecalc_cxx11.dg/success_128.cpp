/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

struct A
{
    // Delegating constructor
    constexpr A(int n) : A(n, n) { }

    constexpr A(int n, int m) : w(n*m) { }
    int w;
};

void f()
{
    static_assert(A(3).w == 9, "Invalid node");
}

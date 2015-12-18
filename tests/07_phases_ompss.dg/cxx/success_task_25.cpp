/*
<testinfo>
test_generator=config/mercurium-ompss
test_CXXFLAGS="--no-copy-deps"
</testinfo>
*/

#include <assert.h>

struct B
{
    int y;
    B( int n ) : y(n) { }
};
struct A
{
    B x;
    A() : x(0) { }
    A(int n) : x(n) { }
};

void f(A &a)
{
    A b;

#pragma omp task inout(a.x) inout(b.x) no_copy_deps
    {
        a.x.y++;
        b.x.y++;
    }
#pragma omp taskwait

    assert(b.x.y == 1);
}

int main(int argc, char *argv[])
{
    A a(41);

    f(a);

    assert(a.x.y == 42);

    return 0;
}

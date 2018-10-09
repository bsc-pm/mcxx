/*
<testinfo>
test_generator=config/mercurium-iomp
</testinfo>
*/

#include <cassert>

struct A {
    int x;

    A(int x_ = 0) : x(x_) {}
    ~A() {}
    A(const A& b) : x(b.x) {}
    A& operator=(const A& b) { x = b.x; return *this; }
    A& operator+(int val) { x += val; return *this; }

};

#pragma omp declare reduction(my_add: A : omp_out = omp_in + omp_out.x) initializer(omp_priv = A(0))
int main(int argc, char *argv[]) {
    A a;
    #pragma omp parallel for reduction(my_add: a)
    for (int i = 0; i < 100; ++i)
    a = a + i;

    assert(a.x == 100/2*99);
}

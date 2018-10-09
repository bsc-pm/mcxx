/*
<testinfo>
test_generator=config/mercurium-iomp
</testinfo>
*/

#include <assert.h>
#include <unistd.h>
#include <omp.h>

/* Same as success_task_reduce_01.cpp but with nonzero init */

struct A {
    int x;

    A(int x_= 77) : x(x_) {}
    ~A() { }
    A(const A& b) : x(b.x) {}
    A& operator=(const A& b) { x = b.x; return *this; }
    A& operator+(int val) { x += val; return *this; }

};

#pragma omp declare reduction(my_add: A : omp_out = omp_in + omp_out.x) initializer(omp_priv = A(2))
int main(int argc, char *argv[]) {
    A a;
    #pragma omp parallel
    #pragma omp single
    {
        #pragma omp taskgroup task_reduction(my_add:a)
        {
            for (int i = 0; i < 10; ++i)
            #pragma omp task in_reduction(my_add:a) firstprivate(i)
            {
                a = a + i;
            }
        }
        // init + loop calc (+ init omp_reduce copies)
        assert(a.x >= 77 + 10/2*9);
    }
}


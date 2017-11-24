/*
<testinfo>
test_generator=config/mercurium-iomp
</testinfo>
*/

/*This tests struct shared/private variables*/

#include <assert.h>

int omp_get_num_threads(void);
int omp_get_thread_num(void);

struct A {
    int x;
};

int main(void) {
    struct A a = { -1 };
    #pragma omp parallel num_threads(4) firstprivate(a)
    {
        a.x++;
    }
    assert(a.x == -1);

    #pragma omp parallel num_threads(4)
    {
        a.x++;
    }
    assert(a.x != -1);

}


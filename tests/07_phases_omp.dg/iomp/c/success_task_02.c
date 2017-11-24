/*
<testinfo>
test_generator=config/mercurium-iomp
</testinfo>
*/

/*This tests task with private vars*/

#include <assert.h>
#include <unistd.h>

int omp_get_num_threads(void);
int omp_get_thread_num(void);

struct C {
    int x;
};

int main(void) {
    int a = 0;
    int b[10] = { 0 };
    struct C c = { 0 };
    #pragma omp parallel
    {
        #pragma omp single
        #pragma omp task firstprivate(a, b, c)
        {
            a++;
            b[0]++;
            c.x++;
        }
    }
    assert(a == 0 && b[0] == 0 && c.x == 0);
}


/*
<testinfo>
test_generator=config/mercurium-iomp
test_compile_fail=yes
test_ignore=yes
test_ignore_reason="Intel RT does not do anything with task priority"
</testinfo>
*/

// This tests task priority. Since Intel/LLVM runtime does not
// do anything with that information there is high chance of fail.
// 
// This test has been run about 50 consecutive times and does not fail
// 
// This test makes three tasks with different priorities whose
// objective is do a dumb loop to loose time. In theory,
// the task with highest priority will end finish, setting
// the variable `first` to its priority.
// This is done num_tests times to check if it is not casuality

#include <string.h>
#include <assert.h>
#include <unistd.h>

#define ACCESS_ONCE(x) (*((volatile typeof(x) *)&(x)))
#define READ_ONCE(x) ACCESS_ONCE(x)
#define WRITE_ONCE(x, val) ({ ACCESS_ONCE(x) = (val); })
#define barrier() __asm__ __volatile__("": : :"memory")

int omp_get_num_threads(void);
int omp_get_thread_num(void);

int main(void) {
    int results[3] = {0};
    int start = 0;
    int first = -1;
    int num_tests = 10000;
    #pragma omp parallel num_threads(5)
    {
        #pragma omp single
        {
            for (int i = 0; i < num_tests; ++i) {
                first = -1;
                start = 0;
                #pragma omp task priority(0)
                {
                    int dumb = 0;
                    while (!READ_ONCE(start));
                    for (int j = 0; j < 100000; ++j)
                        dumb++;
                    if (first == -1) first = 0;
                }
                #pragma omp task priority(1)
                {
                    int dumb = 0;
                    while (!READ_ONCE(start));
                    for (int j = 0; j < 100000; ++j)
                        dumb++;
                    if (first == -1) first = 1;
                }
                #pragma omp task priority(2)
                {
                    int dumb = 0;
                    while (!READ_ONCE(start));
                    for (int j = 0; j < 100000; ++j)
                        dumb++;
                    if (first == -1) first = 2;
                }
                start = 1;

                #pragma omp taskwait
                results[first]++;
            }
        }
    }
    int winner = 0;
    if (results[0] < results[1]) winner = 1;
    if (results[1] < results[2]) winner = 2;

    assert(winner == 2);
}


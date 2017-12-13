/*
<testinfo>
test_generator=config/mercurium-iomp
</testinfo>
*/

/*This tests critical. Also tests if can be two criticals
* at the same time when have different names*/

#include <assert.h>
#include <unistd.h>

#define ACCESS_ONCE(x) (*((volatile typeof(x) *)&(x)))
#define READ_ONCE(x) ACCESS_ONCE(x)
#define WRITE_ONCE(x, val) ({ ACCESS_ONCE(x) = (val); })

#define BOTH_EMPTY -1
#define WAITING_FIRST 0
#define COLLISION 1
#define DONE 2

int main(void) {
    int a = BOTH_EMPTY;
    int collision = 0;

    // force more than 1 threads...
    #pragma omp parallel num_threads(4) 
    {
        #pragma omp critical(bla)
        {
            if (READ_ONCE(a) == BOTH_EMPTY) {
                WRITE_ONCE(a, WAITING_FIRST);
            }
            else if (READ_ONCE(a) != DONE) {
                collision = 1;
                WRITE_ONCE(a, COLLISION);
                while (READ_ONCE(a) == COLLISION);
            }
            
        }
        #pragma omp critical
        {
            while (READ_ONCE(a) == WAITING_FIRST);
            WRITE_ONCE(a, DONE);
        }
    }
    assert(collision == 1);

}

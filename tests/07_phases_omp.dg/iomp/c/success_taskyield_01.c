/*
<testinfo>
test_generator=config/mercurium-iomp
</testinfo>
*/

/*This tests taskyield*/

#include <assert.h>
#include <unistd.h>
#include <omp.h>

omp_lock_t lck1;

int main(void) {
	omp_init_lock(&lck1);
	size_t sum = 0;
	#pragma omp parallel
	#pragma omp single
	{
		for (size_t i = 0; i < 100000; ++i) {
			#pragma omp task
			{
				while (!omp_test_lock(&lck1)) {
					#pragma omp taskyield
				}
				++sum;
				omp_unset_lock(&lck1);
			}
		}
	}
	omp_destroy_lock(&lck1);
	assert(sum == 100000);
}


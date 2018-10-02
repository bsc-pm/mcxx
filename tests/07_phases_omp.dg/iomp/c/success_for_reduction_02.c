/*
<testinfo>
test_generator=config/mercurium-iomp
</testinfo>
*/

#include <assert.h>
#include <stdint.h>

int main(void) {
	int sum = 0;
	#pragma omp parallel
	{
		#pragma omp for reduction(+ : sum) collapse(2)
		for (int32_t j = 0; j < 10; ++j)
			for (int32_t i = 0; i < 20; i++) sum += i;
	}
	assert(sum == 10*20*19/2);
}

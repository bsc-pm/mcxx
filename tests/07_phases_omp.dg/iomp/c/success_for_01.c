/*
<testinfo>
test_generator=config/mercurium-iomp
</testinfo>
*/

#include <assert.h>
#include <stdint.h>

int array[20] = { 0 };

int main(void) {
	#pragma omp parallel
	{
		#pragma omp for
		for (int32_t i = 0; i < 20; i++) array[i] += i;
		#pragma omp for
		for (uint32_t i = 0; i < 20; i++) array[i] += i;

		#pragma omp for
		for (int64_t i = 0; i < 20; i++) array[i] += i;
		#pragma omp for
		for (uint64_t i = 0; i < 20; i++) array[i] += i;
	}
	for (size_t i = 0; i < 20; i++) assert(array[i] == 4*i);
}

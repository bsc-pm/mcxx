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
		#pragma omp for schedule(dynamic)
		for (int32_t i = 0; i < 20; i++) array[i] += i;
		#pragma omp for schedule(dynamic)
		for (uint32_t i = 0; i < 20; i++) array[i] += i;

		#pragma omp for schedule(dynamic)
		for (int64_t i = 0; i < 20; i++) array[i] += i;
		#pragma omp for schedule(dynamic)
		for (uint64_t i = 0; i < 20; i++) array[i] += i;
	}
	for (size_t i = 0; i < 20; i++) assert(array[i] == 4*i);
}

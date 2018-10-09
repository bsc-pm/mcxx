/*
<testinfo>
test_generator=config/mercurium-iomp
test_ignore=yes
test_ignore_reason="VLAs not supported yet"
</testinfo>
*/

#include <assert.h>
#include <stdint.h>
#include <string.h>

int main(int argc, char *argv[]) {
	int sum[argc];
	int b;
	memset(sum, 0, sizeof(sum));
	#pragma omp parallel
	{
		#pragma omp for reduction(+ : sum, b)
		for (int32_t i = 0; i < 20; i++) { sum[0] += 0; b++; }
	}
	for (int32_t i = 0; i < 20; i++)
		assert(sum[i] == i);
}

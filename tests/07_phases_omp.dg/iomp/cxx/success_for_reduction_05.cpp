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
	memset(sum, 0, sizeof(sum));
	#pragma omp parallel
	{
		#pragma omp for reduction(+ : sum)
		for (int32_t i = 0; i < argc; i++) sum[i] += i;
	}
	for (int32_t i = 0; i < argc; i++)
		assert(sum[i] == i);
}

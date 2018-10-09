/*
<testinfo>
test_generator=config/mercurium-iomp
test_ignore_fail=yes
</testinfo>
*/

#include <assert.h>
#include <stdint.h>
#include <omp.h>
#pragma omp declare reduction(myadd: int: omp_out += omp_in) initializer(omp_priv = omp_orig)
int main(void) {
	int sum = 1;
	#pragma omp parallel
	{
		#pragma omp for reduction(myadd : sum)
		for (int32_t i = 0; i < 20; i++) sum += i;
	}
	assert(sum == 20*19/2 + omp_get_max_threads() + 1);
}


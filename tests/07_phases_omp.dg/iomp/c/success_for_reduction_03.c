/*
<testinfo>
test_generator=config/mercurium-iomp
test_ignore_fail=yes
</testinfo>
*/

#include <assert.h>
#include <stdint.h>

struct A {
    int x;
};

#pragma omp declare reduction(my_add: struct A : omp_out.x = omp_in.x + omp_out.x) initializer(omp_priv = omp_orig)
int main(void) {
	struct A a;
	a.x = 0;
	#pragma omp parallel
	{
		#pragma omp for reduction(my_add : a) collapse(2)
		for (int32_t j = 0; j < 10; ++j)
			for (int32_t i = 0; i < 20; i++) a.x += i;
	}
	assert(a.x == 10*20*19/2);
}

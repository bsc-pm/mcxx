/*
<testinfo>
test_generator=config/mercurium-iomp
test_ignore_fail=yes
</testinfo>
*/

#include <assert.h>
#include <stdint.h>
#include <string.h>
struct A {
    int x;
};
#pragma omp declare reduction(my_add: struct A : omp_out.x = omp_in.x + omp_out.x) initializer(omp_priv = A{0})
int main(int argc, char *argv[]) {
	A sum[10];
	for (int32_t i = 0; i < 10; i++) sum[i].x = 0;
	#pragma omp parallel
	{
		#pragma omp for reduction(my_add : sum)
		for (int32_t i = 0; i < 10; i++) sum[i].x += i;
	}
	for (int32_t i = 0; i < 10; i++)
		assert(sum[i].x == i);
}

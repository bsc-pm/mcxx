/*
<testinfo>
test_generator=config/mercurium-iomp
</testinfo>
*/

#include <assert.h>
#include <stdint.h>
#include <string.h>
struct A {
    int x;
    int y;
};

void init(struct A* omp_priv, struct A* omp_orig) {
    omp_priv->x = 0;
    omp_priv->y = omp_orig->y;
}

#pragma omp declare reduction(my_add: struct A : omp_out.x = omp_in.x + omp_out.x) initializer(init(&omp_priv, &omp_orig))
int main(int argc, char *argv[]) {
	struct A sum[10];
	for (int32_t i = 0; i < 10; i++) {
		sum[i].x = 0;
		sum[i].y = 1;
	}
	#pragma omp parallel
	{
		#pragma omp for reduction(my_add : sum)
		for (int32_t i = 0; i < 10; i++) sum[i].x += sum[i].y;
	}
	for (int32_t i = 0; i < 10; i++) {
		assert(sum[i].x == 1);
	}
}

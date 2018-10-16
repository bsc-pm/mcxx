/*
<testinfo>
test_generator=config/mercurium-iomp
</testinfo>
*/

#include <assert.h>
#include <stdint.h>

struct A {
    int x;
    int y;
};

void init(A* omp_priv, A* omp_orig) {
    omp_priv->x = 0;
    omp_priv->y = omp_orig->y;
}

#pragma omp declare reduction(my_add: struct A : omp_out.x = omp_in.x + omp_out.x) initializer(init(&omp_priv, &omp_orig))
int main(void) {
	A a;
	a.x = 0;
	a.y = 1;
	#pragma omp parallel
	{
		#pragma omp for reduction(my_add : a) collapse(2)
		for (int32_t j = 0; j < 10; ++j)
			for (int32_t i = 0; i < 20; i++) a.x += a.y;
	}
	assert(a.x == 10*20);
}

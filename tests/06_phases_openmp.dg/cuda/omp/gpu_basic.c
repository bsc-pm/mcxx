/*
<testinfo>
test_generator=config/mercurium-cuda
compile_versions=cuda_omp
</testinfo>
*/

#include <stdlib.h>
#include "gpu_basic.cu"

#pragma omp target device (cuda) copy_deps
#pragma omp task inout (*a)
void addOne (int *a)
{
#pragma mcc verbatim start
	addOne_gpu <<<1, 1>>> (a);
#pragma mcc verbatim end
}


int main (int argc, char *argv[])
{
	int a = 1;

	addOne(&a);

	if (a != 2) abort();

	return 0;
}

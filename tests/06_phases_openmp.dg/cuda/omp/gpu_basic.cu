/*
<testinfo>
test_ignore=yes
</testinfo>
*/
#ifdef __CUDACC__

__global__ void addOne_gpu(int *a)
{
	*a += 1;
}

#endif


/*
<testinfo>
test_generator=config/mercurium-nanox
</testinfo>
*/

template <typename T>
void copy(T* dst, T* src, int N)
{
    for(int i = 0; i < N; i++)
    {
#pragma omp task shared(dst, src)
        dst[i] = src[i];
    }
#pragma omp taskwait
}

void f(void)
{
    int a[10], b[10];

    copy(a, b, 10);
}

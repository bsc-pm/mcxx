/*
 <testinfo>
 test_generator=config/mercurium-ompss-cuda
 test_noexec=yes
 </testinfo>
 */
#pragma omp target device(smp, cuda)
typedef int kaka_t;
int main()
{
    #pragma omp task
    {}
}

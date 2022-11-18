/*
<testinfo>
test_generator=("config/mercurium-ompss-2 openmp-compatibility")
test_ignore="yes"
test_ignore_reason="nanos6 does not have support for this yet"
</testinfo>
*/


int main(int argc, char *argv[])
{
    int N = 10;
    int *A = (int *)malloc(N * sizeof(int));
   
    #pragma omp task out(A[0]) none(A[1])
    {
    
    }

    #pragma omp taskwait

    return 0;
}

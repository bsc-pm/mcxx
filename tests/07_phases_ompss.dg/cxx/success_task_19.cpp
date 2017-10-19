/*
<testinfo>
test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
</testinfo>
*/

void foo(int n)
{
    #pragma omp task
    {
    }
    #pragma omp taskwait
}

int main()
{
    #pragma omp task
    {
        foo(1);
        foo(2);
    }
    #pragma omp taskwait
}

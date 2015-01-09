/*
<testinfo>
test_generator=config/mercurium-ompss
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

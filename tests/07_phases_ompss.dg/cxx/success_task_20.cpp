/*
<testinfo>
test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
</testinfo>
*/
#pragma omp task
void foo();

void bar()
{
    foo();
    #pragma omp taskwait
}

void foo()
{
    #pragma omp task
    {
    }
    #pragma omp taskwait
}

int main()
{
    foo();
}

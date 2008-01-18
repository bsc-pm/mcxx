void f(void)
{
#pragma omp parallel
    {
#pragma omp task
        {
        }
    }
}

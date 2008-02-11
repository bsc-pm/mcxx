void f(void)
{
#pragma omp sections
    {
#pragma omp section
        {
        }
#pragma omp section
        {
        }
    }
}

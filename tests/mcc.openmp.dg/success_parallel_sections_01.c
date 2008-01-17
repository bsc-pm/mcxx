void f(void)
{
#pragma omp parallel sections
    {
#pragma omp section
        {
        }
#pragma omp section
        {
        }
#pragma omp section
        {
        }
    }
}

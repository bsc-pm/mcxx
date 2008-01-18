void f(void)
{
#pragma omp parallel for
    for (int i = 0; i < 10; i++)
    {
#pragma omp task
        {
        }
    }
}

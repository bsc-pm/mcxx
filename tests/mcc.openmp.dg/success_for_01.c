void f(void)
{
    int i;
#pragma omp for
    for (i = 0; i < 100; i++)
    {
    }
}

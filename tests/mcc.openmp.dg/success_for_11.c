void f(void)
{
    int i;
    int s = 0;
#pragma omp for reduction(+:s)
    for (i = 0; i < 10; i++)
    {
        s = s + 1;
    }
}

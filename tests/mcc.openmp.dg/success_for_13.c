void f(void)
{
    int x, k;
#pragma omp parallel private(x)
    {
#pragma omp for
        for (x = 0; x < 10; x++)
        {
            k = 3;
        }
    }
}

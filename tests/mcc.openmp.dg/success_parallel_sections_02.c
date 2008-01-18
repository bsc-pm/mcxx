void f(void)
{
    int s = 0;

#pragma omp parallel sections reduction(+:s)
    {
#pragma omp section
        {
            s = s + 1;
        }
#pragma omp section
        {
            s = s + 1;
        }
#pragma omp section
        {
            s = s + 1;
        }
    }
}

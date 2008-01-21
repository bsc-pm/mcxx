void f(void)
{
    int i;
#pragma omp transaction
    {
        i = 0;
    }
}

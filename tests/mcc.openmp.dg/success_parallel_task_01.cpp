void f(void)
{
    int k = 1;
#pragma omp parallel private(k)
    {
        k = 2;
#pragma omp task firstprivate(k)
        {
            k = 3;
        }
    }
}

void g(void)
{
    int k = 1;
#pragma omp parallel private(k)
    {
        k = 1;
#pragma omp parallel firstprivate(k)
        { 
            k = 2;
        }
    }
}

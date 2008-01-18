void f(void)
{
    int i = 3;

#pragma omp atomic
    i = i + 1;

}

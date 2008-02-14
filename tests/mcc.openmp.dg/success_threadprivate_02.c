int a = 3;

#pragma omp threadprivate(a)

void f(void)
{
    a = 3;
}

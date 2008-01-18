int a[10];

void f(void)
{
    int b[10];
    int i;

#pragma omp for firstprivate(a, b)
    for (i = 0; i < 10; i++)
    {
        a[1] = a[2] + 3;
        b[4] = b[5] + 6;
    }
}

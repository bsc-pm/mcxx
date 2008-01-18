int a;

void f(void)
{
    int b;
    int i;

#pragma omp for private(a, b)
    for (i = 0; i < 10; i++)
    {
        a = a + 3;
        b = b + 4;
    }
}

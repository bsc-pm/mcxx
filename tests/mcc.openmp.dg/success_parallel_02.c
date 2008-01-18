int a;
void f(void)
{
    int b;
#pragma omp parallel shared(a, b)
    {
        a = a + 3;
        b = b + 4;
    }
}

int a[10];
void f(void)
{
    int b[10];
#pragma omp parallel private(a, b)
    {
        a[2] = a[3] + 4;
        b[5] = b[6] + 7;
    }
}

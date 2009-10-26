void f(void)
{
    int n = 10, m = 20;
    int v[n + 1][m * 2];

#pragma omp task shared(v)
    {
        v[n-1][m-1] = 3;
    }
}

void g(void)
{
    int n = 10, m = 20;
    int v[n + 1][m * 2];

#pragma omp parallel shared(v) firstprivate(n, m)
    {
        v[n-1][m-1] = 3;
    }
}

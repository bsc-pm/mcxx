/*
<testinfo>
test_generator=config/mercurium-ss2omp
</testinfo>
*/

#pragma css task input(n) inout(m)
void f(int n, int m[n][n])
{
    int i, j;
    for (i = 0; i < n; i++)
    {
        for (j = 0; j < n; j++)
        {
            m[i][j]++;
        }
    }
}

void g(int m, int a[m][m])
{
    f(m, a);
}

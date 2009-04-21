void f(void)
{
    int i, j, k;
    int **a;

#pragma hlt interchange permutation(2, 1)
    for (i = 0; i < 100; i++)
    {
        k = 3;
        for (j = 0; j < 200; j++)
        {
            a[i][j] = i + j;
        }
    }
}

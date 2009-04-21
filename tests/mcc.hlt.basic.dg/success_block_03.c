void f(void)
{
    int a[100][100];

    int n = 10;
    int m = 20;
#pragma hlt block factors(n, m)
    for (int i = 0; i < 100; i++)
    {
        for (int j = 0; j < 100; j++)
        {
            a[i][j] = i * j;
        }
    }
}

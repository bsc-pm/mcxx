void f(void)
{
    int a[100][100];

    int i;
    int j;

#pragma hlt block factors(10, 20)
    for (i = 0; i < 100; i++)
    {
        for (j = 0; j < 100; j++)
        {
            a[i][j] = i * j;
        }
    }
}

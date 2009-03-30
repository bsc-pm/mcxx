int g(int n);

void f(void)
{
    int a[100], b[100], c[100];

    int i, j;

#pragma hlt fusion
    {
        for (i = 0; i < 100; i++)
        {
            a[i] = 0;
        }
        c[3] = 4;
        for (j = 0; j < 100; j++)
        {
            b[j] = 0;
        }
    }
}

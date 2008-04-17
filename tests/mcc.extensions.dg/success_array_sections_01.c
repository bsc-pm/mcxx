int a[100];
int *b;

void f(void)
{
    a[0:49] = 1;
    a[50:99] = 2;

    b = a;

    b[0:49] = 2;
    b[50:99] = 3;
}

void f(void)
{
    int *p, *q;

    q = p + 1;
    q = p - 1;

    q++;
    ++q;

    q--;
    --q;

    int a;

    q = p + a;
}

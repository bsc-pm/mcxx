void f(void)
{
    int *p;

    *p = 3;
    *p = 3.4;
    *p = 3.4f;

    p[1] = 3;
    p[1] = 3.4;
    p[1] = 3.4f;
}

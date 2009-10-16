void f(a, b);

void g(void)
{
    void (*p)(int, int);
    p = f;
}

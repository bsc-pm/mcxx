struct A
{
    A();
    void foo(A&);
};

#pragma omp declare reduction type(A) operator(.foo)

void f(void)
{
    A v[10];
    A s;

    int i;

#pragma omp parallel for reduction(.foo : s)
    for (i = 0; i < 10; i++)
    {
        s.foo(v[i]);
    }
}

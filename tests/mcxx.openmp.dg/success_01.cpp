namespace A
{
    int a;
    int b;
};

void f(void)
{
#pragma omp parallel firstprivate(A::a) shared(A::b)
    {
        A::a = 4;
        A::b = 3;
    }
}

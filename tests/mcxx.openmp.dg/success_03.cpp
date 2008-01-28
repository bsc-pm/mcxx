int a;

struct A
{
    int b;
    static int c;

    void f(void)
    {
#pragma omp parallel
        {
            a = 3;
            b = 3;
            c = 3;
        }
    }
};

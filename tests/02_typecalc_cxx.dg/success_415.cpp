/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

float *test(int&);
double *test(const int&);

struct A
{
    void f(const int c);
    void g(int c);
};


void A::f(int c)
{
    c += 3;

    float *pf;
    pf = test(c);
}


void A::g(const int c)
{
    double *pd;
    pd = test(c);
}

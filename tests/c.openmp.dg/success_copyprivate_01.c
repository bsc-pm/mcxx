/*
<testinfo>
test_generator=config/mercurium-omp
</testinfo>
*/
int d;
#pragma omp threadprivate(d)

void f1(void)
{
#pragma omp parallel copyprivate(d)
    {
        d = d + 3;
    }
}

int c[1];
#pragma omp threadprivate(c)

void f2(void)
{
#pragma omp parallel copyprivate(c)
    {
        c[0] = c[0] + 3;
    }
}

/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
double* my_test1(const volatile int*);
float* my_test1(volatile int *);

void foo()
{
    int x;
    float *f = my_test1(&x);

    const int y = 1;
    double *d = my_test1(&y);
}

double* my_test2(const volatile void*);
float* my_test2(volatile void *);

void bar()
{
    int x;
    float *f = my_test2(&x);

    const int y = 1;
    double *d = my_test2(&y);
}

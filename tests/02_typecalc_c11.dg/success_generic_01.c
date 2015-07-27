/*
<testinfo>
test_generator="config/mercurium-c11"
</testinfo>
*/

void f_int(int*);
void f_float(float*);
void f_double(double*);

#define f(x) _Generic((x), \
        int* : f_int, \
        float* : f_float, \
        double* : f_double ) \
        (x)

void g()
{
    int *pi = 0;
    float *pf = 0;
    double *pd = 0;
    char *pc = 0;

    f(pi);
    f(pf);
    f(pd);

    float a[10];
    f(a);
}


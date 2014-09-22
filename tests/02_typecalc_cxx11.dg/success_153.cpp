/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

template <typename ...S>
float* f(S...);

template <typename T, typename ...S>
double* f(T, S...);

void g()
{
    float *pf1 = f();
    double *pf2 = f(1, 2);
    double *pf3 = f(1, 2, 3);
}

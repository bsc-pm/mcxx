/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T>
double* f(const T* const &);

template <typename T>
float* f(T*);

void g(const int *a)
{
    double *d;
    d = f(a);
}

/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T>
struct A { };

template <typename T>
float* foo(T& a);

template <typename T>
double* foo(const T& a);

void g(const A<int>* b)
{
    double *d;
    d = foo(*b);
}

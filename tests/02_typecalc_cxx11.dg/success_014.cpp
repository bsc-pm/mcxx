/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <typename ...Q>
struct A { };

template <typename T>
A<T*>* g(A<T> *, T *);

void f()
{
    A<float> *pa;
    float *pf;
    A<float*> *ppa;

    ppa = g(pa, pf);
}

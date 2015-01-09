/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <typename ...T>
struct A
{
    void g(T* ...t);
};

template <typename T, typename S>
struct B
{
    void g(T*, S*);
};

void f()
{
    A<float, double> a;
    B<float, double> b;

    float *pf;
    double *pd;
    a.g(pf, pd);
    b.g(pf, pd);
}

/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

namespace A
{
    int i;
    int* g(const int&);
    float* g(const int&&);

    int f1();
    int&& f2();

    void f()
    {
        int *pi;
        pi = g(i);

        float* pf;
        pf = g(f1());
        pf = g(f2());
    }

}

namespace B
{
    int* f(void(&)());
    float* f(void(&&)());
    void g();

    void h()
    {
        int* pi;
        pi = f(g);
    }
}

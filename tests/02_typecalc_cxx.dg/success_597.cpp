/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename NumType>
struct A
{
    typedef NumType* __restrict__ Vector;

    inline void nvector1(Vector* w, const int n) { }
    // inline void nvector2(NumType* __restrict__* w, const int n) { }
};

void f()
{
    A<double> d;

    double *__restrict* w = 0;

    d.nvector1(w, 1);
}

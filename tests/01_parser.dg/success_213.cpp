/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

struct A
{
    template <int N>
        void f(int m);
    template <int N, int M>
        void f(int m);
};

template <typename T>
void f(A *a)
{
    T b;

    b + a->f< 1 >(2);
    b + a->f< 3, 4 >(2);
}

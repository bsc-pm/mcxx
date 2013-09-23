/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T>
struct A
{
    template <int N>
        void f(T (&v)[N])
        {
        }
};

void g()
{
    A<float> a;
    float v[10];

    a.f(v);

}

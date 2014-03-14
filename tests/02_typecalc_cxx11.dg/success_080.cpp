/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <typename ...T>
struct A
{
    template <typename ...S>
        using B1 = void (*)(T..., S...);

    template <typename ...S>
        using B2 = void (*)(S..., T...);
};

void g()
{
    void (*p1)(int, float, double, char);
    A<int, float>::B1<double, char> p2;

    p1 == p2;

    void (*p3)(double, char, int, float);
    A<int, float>::B2<double, char> p4;

    p3 == p4;
}

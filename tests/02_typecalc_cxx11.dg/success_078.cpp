/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <typename ...T>
using A = void (*)(T...);

void g()
{
    A<int, float, double> a;
    void (*p)(int, float, double);

    a = p;
}

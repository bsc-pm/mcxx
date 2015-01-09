/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <typename ...T>
void f(int (&...a)[sizeof(T)]);

void g()
{
    int a[sizeof(double)];
    int b[sizeof(float)];

    f<double, float>(a, b);
}

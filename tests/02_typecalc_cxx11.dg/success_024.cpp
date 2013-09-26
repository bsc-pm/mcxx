/*
<testinfo>
test_generator="config/mercurium"
test_CXXFLAGS="-std=c++11"
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

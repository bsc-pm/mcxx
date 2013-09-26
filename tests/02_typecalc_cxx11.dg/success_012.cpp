/*
<testinfo>
test_generator="config/mercurium"
test_CXXFLAGS="-std=c++11"
</testinfo>
*/

template <typename ...T>
struct A
{
};

template <typename ...T>
void h(A<T...> *a);

void f()
{
    A<float, double> a;
    h(&a);
}

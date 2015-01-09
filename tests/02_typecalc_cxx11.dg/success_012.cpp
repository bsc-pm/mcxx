/*
<testinfo>
test_generator="config/mercurium-cxx11"
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

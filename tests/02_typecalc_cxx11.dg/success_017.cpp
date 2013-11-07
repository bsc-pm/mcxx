/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <typename T1, typename T2, typename T3 = void(T1, T2)>
struct A { };

template <typename ...Q>
void f(void (*p)(Q ...q), A<Q..., double> *a);

void h(int, float);
void g()
{
    A<int, float, double> a;
    f(h, &a);
}


/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <typename T1, typename T2, typename T3 = void(T1, T2)>
struct A { };

template <typename ...Q>
void f(void (*p)(Q ...q), A<Q..., Q*...> *a);

void h(int);
void g()
{
    A<int, int*> a;
    f(h, &a);
}


/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <typename ...T>
struct A {};

template <typename T, typename S>
void f(A<T, S>* a);

void g()
{
    A<int, float> *pA;
    ::f(pA);
}

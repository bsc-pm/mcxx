/*
<testinfo>
test_generator="config/mercurium"
test_CXXFLAGS="-std=c++11"
</testinfo>
*/

template <typename ...T>
void f(T* ...a);

template <typename T>
struct A { };

void g_test()
{
    A<int>* pi = 0;
    A<float>* pf = 0;

    ::f(pi, pf);
}

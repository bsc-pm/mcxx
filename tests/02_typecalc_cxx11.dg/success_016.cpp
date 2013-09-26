/*
<testinfo>
test_generator="config/mercurium"
test_CXXFLAGS="-std=c++11"
</testinfo>
*/

template <typename T>
struct A { };

template <typename ...T>
void g(A<T>* ...a);

void g_test()
{
    A<int>* pi = 0;
    A<float>* pf = 0;

    ::g(pi, pf);
}

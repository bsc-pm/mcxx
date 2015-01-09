/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <typename ...T>
void f(T* ...a);

void f_test()
{
    int* pi = 0;
    float* pf = 0;

    ::f(pi, pf);
}


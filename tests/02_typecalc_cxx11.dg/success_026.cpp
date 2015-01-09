/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <typename T = int, T N>
void f(int (&a)[N]);

void g()
{
    int a[100];
    ::f(a);
    ::f<>(a);
    ::f<int>(a);
    ::f<int, 100>(a);
}

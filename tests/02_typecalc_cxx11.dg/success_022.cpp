/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <int ...N>
void f(int (&...a)[N]);

void g()
{
    int a[10];
    int b[20];

    ::f(a, b);
}

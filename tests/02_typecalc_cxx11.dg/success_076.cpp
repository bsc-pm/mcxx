/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <typename T>
struct A
{
    template <typename S = T*>
    S f();
};

void g()
{
    A<int> a;
    int * pi;
    pi = a.f();
}

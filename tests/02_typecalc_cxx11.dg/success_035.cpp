/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/
template <typename T, typename ...S>
struct A
{
    T t;

    void f(S ...s);
};

void g()
{
    A<int> a;
    a.f();
}

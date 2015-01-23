/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

template <typename T>
void f1()
{
    auto m = T();
    m();
}

template <typename T>
void f2()
{
    int x;
    auto g = [&]() { x++; };
    g();
}

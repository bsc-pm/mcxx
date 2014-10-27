/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

template <typename ...T>
void f(T&& ...args)
{
}

void g(const int& n, const float& m)
{
    f(n);
    f(n, n);

    f(m);
    f(m, m);

    f(n, m);
    f(m, n);
}

struct A
{
template <typename _T>
    void f(_T);
};

void g()
{
    A a;

    a.f(3);
}

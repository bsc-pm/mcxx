namespace A
{
    typedef int T;
}

void f(int*);

void g(void)
{
    using namespace A;
    T* t;

    t = 0;

    f(t);
}

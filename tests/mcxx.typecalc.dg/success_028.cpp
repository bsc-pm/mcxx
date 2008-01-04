struct A
{
    int m;
};

void h(int)
{
}

void f(int A::*pm)
{
    A a;
    a.*pm = 3;

    h(a.*pm);
}

void g(int A::*pm)
{
    A * p_a;
    p_a->*pm = 3;

    h(p_a->*pm);
}


void g()
{
    int A::* p = &A::m;

    f(p);
    g(p);
}

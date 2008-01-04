struct B
{
    int m;
};

struct A
{
    B* operator->();
};

void f()
{
    A a;

    int n;
    n = a->m;
}

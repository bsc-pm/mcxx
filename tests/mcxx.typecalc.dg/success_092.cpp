struct A
{
    int* g();
};

struct B
{
    A _a;
    A& operator*()
    {
        return _a;
    }

    A* operator->()
    {
        return &_a;
    }
};

void g(int *);

void f()
{
    B b;
    int *k;

    g(b->g());
}

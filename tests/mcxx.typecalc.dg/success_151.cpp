struct A
{
    int a;
};

struct B
{
    A a;
};


void f(B *b)
{
    (*b).B::~B();
}


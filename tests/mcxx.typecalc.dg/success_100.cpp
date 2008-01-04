struct A
{
    void f();
};

struct B : public A
{
};

struct C : public B
{
    void f() { B::f(); }
};

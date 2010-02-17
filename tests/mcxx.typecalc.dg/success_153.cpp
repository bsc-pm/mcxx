struct B;

struct A
{
    A(B*);
};

struct B
{
    A a;

    B()
     : a(this)
    {
    }
};

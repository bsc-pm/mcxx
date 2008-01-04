struct A
{
    int a;
};

struct B : virtual A
{
};

struct C : virtual A
{
};

struct D : B, C
{
    void f ()
    {
        a = 3;
    }
};

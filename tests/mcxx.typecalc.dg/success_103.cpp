struct A
{
    int x;
};

struct B : virtual A
{
};

struct C : B
{
};

struct D : B
{
};

struct E : C, D
{
    void f()
    {
        x = 3;
    } 
};


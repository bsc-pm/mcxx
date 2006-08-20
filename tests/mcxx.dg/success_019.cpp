struct A
{
};

struct B : virtual A
{
};

struct C : virtual A
{
};

struct D : B, C
{
    D()
        : A(), B(), C()
    {
    }
};

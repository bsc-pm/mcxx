struct M
{
    int x;
};

struct C : M
{
};

struct A : virtual C
{
};

struct B : virtual C
{
};

struct D : A, B
{
    void f()
    {
        x = 1;
    }
};

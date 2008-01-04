namespace C
{
struct A
{
};
}

struct B : C::A
{
    B() : A() { }
};

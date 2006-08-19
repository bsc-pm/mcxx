struct C 
{
};

struct A  : public C
{ 
    A()
    {
    }
};


struct B : public A
{
    B() 
        : A() 
    {
    }
};

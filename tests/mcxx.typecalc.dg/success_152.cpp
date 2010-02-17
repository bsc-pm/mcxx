struct OneType
{
    OneType();
};

struct A
{ 
    A();
    A& operator=(const A&); 

    operator OneType() const { }
};

struct B : A
{ 
    B();

    using A::operator OneType;
    operator OneType() { }
};

void g(OneType);

void f(const B b)
{
    g(b);
}

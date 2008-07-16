struct A
{
};

struct B : A
{
    B(const B&);
    B(const A&);

    void operator +(int);
};

void f(B a)
{
    B(a) + 3;
}

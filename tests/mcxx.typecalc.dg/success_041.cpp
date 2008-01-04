struct A
{
    void f();
    int *f() const;

    void g() const;
};

void A::g() const
{
    int *k;
    k = f();
}

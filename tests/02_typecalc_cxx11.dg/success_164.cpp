/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/
struct A
{
    int x;
    void f1();
    void f2() const;

    void g() const;
};

void A::f1()
{
    [&](A* t) { this->x = 0; t->g(); };
}

void A::f2() const
{
    [&](A* t) { int y; y = this->x; t->g(); };
}

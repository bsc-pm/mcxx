/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

struct C
{
};

struct D : C
{
};

struct A
{
    virtual void f() &;
    virtual void f() &&;

    virtual C* g();
    virtual C& h();
};

struct B : A
{
    void f() & override;
    void f() && override;

    D* g() override;
    D& h() override;
};

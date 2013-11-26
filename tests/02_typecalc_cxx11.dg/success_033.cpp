/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

struct A
{
    void nonref();
    void lvalue() &;
    void rvalue() &&;

    A(int);
};

void g()
{
    A a(3);

    a.nonref();
    a.lvalue();

    A(3).nonref();
    A(3).rvalue();
}

/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

struct A
{
    A();
    explicit operator bool();
};

bool operator==(const A&, const A&);

void f()
{
    A a;
    true || a;
    true && a;
    a || true;
    a && true;
}

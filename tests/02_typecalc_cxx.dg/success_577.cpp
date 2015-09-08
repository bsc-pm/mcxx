/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
struct A
{
    A(void(*)());
};

void h();
void f(const A&);

void g()
{
    f(h);
}

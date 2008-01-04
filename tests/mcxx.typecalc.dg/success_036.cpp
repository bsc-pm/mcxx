struct A
{
    A& f();
    A& g();
    A& h();
};

void f()
{
    A a;

    a.f().g().h();
}

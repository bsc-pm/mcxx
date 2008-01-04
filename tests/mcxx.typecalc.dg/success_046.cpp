struct A
{
};

void f()
{
    A *p_a;
    p_a->~A();

    A a;
    a.~A();
}

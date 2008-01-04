struct A
{
    A& operator=(const A&);
};

void f()
{
    A a, b;
    a = b;
}

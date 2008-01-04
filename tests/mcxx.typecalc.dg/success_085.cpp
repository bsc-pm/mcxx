struct A
{
    operator int&();
};

void f(int);

void g()
{
    A a;

    f(a++);
    f(++a);

    f(--a);
    f(a--);
}

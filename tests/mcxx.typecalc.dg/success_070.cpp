void f(int, float = 3.4);

void g()
{
    f(2);
}

void f(int = 1.4, float);

void g2()
{
    f();
}

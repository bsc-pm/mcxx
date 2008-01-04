struct A
{
    A(int, float);
};

void f()
{
    A *a;

    a = new A(1, 2.3f);
}

struct A
{
    operator int();
};

void f()
{
    A a;
    int n;
    char c;

    n = a;
    c = a;
}

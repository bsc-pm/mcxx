void f(int&);

void g()
{
    int a, b;
    bool c;

    f(c ? a : b);
}

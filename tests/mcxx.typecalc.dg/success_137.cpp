struct A
{
    operator bool();

    A(int);
};

void f()
{
    bool b;

    A(3) && b;
}

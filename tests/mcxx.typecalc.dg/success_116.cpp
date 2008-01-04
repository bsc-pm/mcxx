void f(int (&)(int));

struct A
{
    static int h(int);
};

void g()
{
    f(A::h);
}

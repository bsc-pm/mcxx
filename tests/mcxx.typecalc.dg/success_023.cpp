struct A
{
    operator int();
};

void h(float);

void g();
float g(int a);

void f()
{
    A a;
    h(g(a));
}

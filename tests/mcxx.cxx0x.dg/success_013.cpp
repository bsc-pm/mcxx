void f(int&&);

void g(int &a)
{
    f(a);
}

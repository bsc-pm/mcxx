void f(int (&)(int));

int k(int);

void g()
{
    f(k);
}

struct A
{
    int* operator[](int);
};

void f(int*);

enum { E = 3 };

void g()
{
    A a;

    f(a[10]);
    f(a[E]);
}

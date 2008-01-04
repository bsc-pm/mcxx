struct A
{
    float* operator()(int *);
};

void g(float*);

void h()
{
    A a;

    int *p;

    g(a(p));
}

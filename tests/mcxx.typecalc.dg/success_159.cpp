struct A;

struct B
{
    int* operator[](const A&);
    float* operator[](unsigned int);

    void f();
};

void B::f()
{
    B b;

    float *pf;
    pf = b[3];
}

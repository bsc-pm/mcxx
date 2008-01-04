struct A
{
    int* f(float*);

    int* m(int*);
    int* m(float*);
};

void g(int* (A::*)(float*));

void h()
{
    g(&A::m);
}

struct A
{
    static float* f(float);
    static int* f(int);
};

void g()
{
    int* i;
    i = A::f(3);

    float *f;
    f = A::f(3.4f);
}

void f1(int*);
void f2(float*);

struct A
{
    float* g() const;
    int* g();
};

void h(void)
{
    A a;
    f1(a.g());

    const A &b = a;
    f2(b.g());
}

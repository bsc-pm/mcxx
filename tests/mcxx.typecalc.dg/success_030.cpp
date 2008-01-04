struct A
{
    int* f();
    float* f() const;
};

void f1(int*);
void f2(float*);

typedef const A C_A;
typedef const C_A CC_A;

void g()
{
    C_A c_a = A();
    f2(c_a.f());

    CC_A cc_a = A();
    f2(c_a.f());
}

struct A
{
    float* operator++(int);
    void* operator--(int);

    int* operator++();
    bool* operator--();
};

void f(float*);
void v(void*);

void i(int*);
void b(bool*);

void g()
{
    A a;

    f(a++);
    v(a--);

    i(++a);
    b(--a);
}

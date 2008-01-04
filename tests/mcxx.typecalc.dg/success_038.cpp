namespace A
{
    template <typename _T>
    _T f(_T);
}

int* h(int);
float* h(long);

void f1(int*);
void f2(float*);

void g()
{
    f1(h(A::f<int>(3l)));
    f1(h(A::f(3)));

    f2(h(A::f(3l)));
}

struct A
{
    template <typename _T>
    int* f(_T*);
};

void g(int* (A::*)(float*));

void h()
{
    g(&A::f<float>);
}

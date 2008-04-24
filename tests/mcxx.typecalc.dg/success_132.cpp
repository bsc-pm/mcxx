typedef float* (*fp1)(int);
typedef char* (*fp2)(double);

struct A
{
    operator fp1();
    operator fp2();

    double* operator()(char*);
};

void f()
{
    A a;

    float* pf; 
    pf =  a(3);

    char* pc;
    pc = a(3.4);

    double * pd;
    pd = a(pc);
}

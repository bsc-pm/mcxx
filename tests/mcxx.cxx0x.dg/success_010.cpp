int *f(int&);
float *f(int&&);

void g(int*);
void h(float*);

int s();

void m()
{
    int a;

    g(f(a));

    h(f(s()));
    h(f(3));
}

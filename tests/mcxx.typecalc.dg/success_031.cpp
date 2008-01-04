void f(int*);
void f(float*);

void h(void (*)(int*));

void g()
{
    void (*k)(int*);

    k = f;

    h(f);
}

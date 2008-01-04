char h(void);

float g(int);

void f(void)
{
    h();

    g(3);
    g(3.4);
    g(3.4f);

    long int (*k)(int, float);

    (*k)(1, 23);
    (*k)(1, 2.3);
    (*k)(1.2f, 3.4f);
}

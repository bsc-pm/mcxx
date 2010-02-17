extern int a[];
enum { B };

void f()
{
    int *c;
    c = a + B;
}

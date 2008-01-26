int* f(bool b);
float* f(void* b);

void g(const char *c)
{
    int* c1;
    c1 = f("a");
    int* c2;
    c2 = f(c);
}

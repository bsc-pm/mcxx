int* f(const char*);
float* f(char *);

void g()
{
    int * pi;
    pi = f("a");
}

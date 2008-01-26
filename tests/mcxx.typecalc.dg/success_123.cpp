int* f(void*);
float* f(bool);

void g(char* c)
{
    int *i;
    i = f(c);
}

int N = 4;
#pragma hlt extend factor(N)
void f(int *a)
{
    *a = 3;
}

#pragma hlt extend factor(25 + 1)
void g(int *b)
{
    *b = 3;
}

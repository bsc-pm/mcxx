int f(int &&a);

void g(int &&a)
{
    f(a);
}

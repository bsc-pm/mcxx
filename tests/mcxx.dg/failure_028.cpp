template <int n>
struct A
{
    int f(int& k)
    {
        k = n;
        return k;
    }
};

void g()
{
    int m;
    int s = A<int>::f(m);
}

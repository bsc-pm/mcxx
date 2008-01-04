template <typename _T>
void f(const _T &, const _T&);

void g()
{
    const int n = 10;
    f(n, int(n));
}

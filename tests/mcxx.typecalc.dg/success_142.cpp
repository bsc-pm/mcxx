template <typename _T>
void f(const _T*);

void g()
{
    int *k;

    f(k);
}

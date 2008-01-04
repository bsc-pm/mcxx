void f(int *);

template <typename _T>
_T *g(_T t);

void h()
{
    f(g(3));
}

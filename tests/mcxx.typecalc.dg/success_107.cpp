template <int _N>
void f(int (*a)[_N]);

template <typename _T>
void f(_T t);

void g()
{
    f<int>(3);
}

template <typename _T>
void f(_T, _T);

const int m();

typedef const int CINT;

void g()
{
    const int a = 10;

    f(a, CINT(a));
}

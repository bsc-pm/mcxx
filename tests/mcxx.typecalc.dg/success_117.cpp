template <typename _T>
void f(_T (&)(_T));

int h(int);

void g()
{
    f(h);
}

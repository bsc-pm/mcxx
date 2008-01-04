template <typename _T, typename _Q>
void f(_T t, _Q q);

void g()
{
    f<int>(3.4f, 2);
}

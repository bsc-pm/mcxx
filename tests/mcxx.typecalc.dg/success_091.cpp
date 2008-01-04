template <typename _T, typename _Q, typename _S>
void f(_T);

template <typename _T>
void g(_T t)
{
    f<_T>(3);
    f<int>(t);
}

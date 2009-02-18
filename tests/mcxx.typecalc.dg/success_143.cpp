template <typename _Ret, typename _T1> 
void f( _Ret (*)(_T1), _Ret, _T1);

template <typename _Ret, typename _T1> 
_Ret h(_T1);

void m()
{
    int i;
    float g;

    f(h, i, g);
}

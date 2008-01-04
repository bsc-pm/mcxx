template <typename _Comparator, typename _T>
int f(_Comparator _comp, _T a, _T b)
{
    return _comp(a, b);
}

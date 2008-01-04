template<typename _Tp>
inline const _Tp& my_min(const _Tp& __a, const _Tp& __b)
{
    if (__b < __a)
        return __b;
    return __a;
}

int f(const int &a, const int &b)
{
    return my_min(a, b);
}

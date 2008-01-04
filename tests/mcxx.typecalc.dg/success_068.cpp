template <typename _T>
struct A
{
    friend void _T::template f<>(_T);
};

template <typename _T>
struct my_is_integer
{
    enum { __value = false };
};

template <>
struct my_is_integer<int>
{
    enum { __value = false };
};

template<typename _Tp, bool = my_is_integer<_Tp>::__value>
struct __promote
{ typedef double __type; };

template<typename _Tp>
struct __promote<_Tp, false>
{ typedef _Tp __type; };

template<typename _Tp, typename _Up>
struct __promote_2
{
    private:
        typedef typename __promote<_Tp>::__type __type1;
        typedef typename __promote<_Up>::__type __type2;

    public:
        typedef __typeof__(__type1() + __type2()) __type;
};

template <typename _T>
struct my_complex
{
};


template<typename _Tp, typename _Up>
struct __promote_2<my_complex<_Tp>, _Up>
{
    public:
        typedef my_complex<typename __promote_2<_Tp, _Up>::__type> __type;
};

template<typename _Tp, typename _Up>
struct __promote_2<_Tp, my_complex<_Up> >
{
    public:
        typedef my_complex<typename __promote_2<_Tp, _Up>::__type> __type;
};

template<typename _Tp, typename _Up>
struct __promote_2<my_complex<_Tp>, my_complex<_Up> >
{
    public:
        typedef my_complex<typename __promote_2<_Tp, _Up>::__type> __type;
};

/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template<typename _Alloc, typename _Tp>
class __alloctr_rebind_helper
{
    template<typename _Alloc2, typename _Tp2>
        static constexpr bool
        _S_chk(typename _Alloc2::template rebind<_Tp2>::other*)
        { return true; }

    template<typename, typename>
        static constexpr bool
        _S_chk(...)
        { return false; }
    public:
    static const bool __value = _S_chk<_Alloc, _Tp>(nullptr);
};

struct A
{
    template <typename T>
        struct rebind
        {
            typedef T other;
        };
};

struct B { };

template <bool _B>
struct C
{
    typedef int False;
};

template <>
struct C<true>
{
    typedef int True;
};

void f(void)
{
    C<__alloctr_rebind_helper<int, float>::__value>::False f;
    C<__alloctr_rebind_helper<A, B>::__value>::True t;
}


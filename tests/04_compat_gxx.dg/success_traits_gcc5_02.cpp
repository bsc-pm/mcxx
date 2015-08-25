/*
<testinfo>
test_generator=config/mercurium-fe-only
test_CXXFLAGS="-std=c++11 --pp=off"
</testinfo>
*/

template <bool>
struct A;

template <>
struct A<true>
{
    typedef int type;
};

template<typename _Tp, typename... _Args>
struct is_trivially_constructible
: public A<__is_trivially_constructible(_Tp, _Args...)>::type
{ };

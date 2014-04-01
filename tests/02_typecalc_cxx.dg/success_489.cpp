/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <bool _B>
struct B;

template <typename _Tp>
struct A : B<(static_cast<bool>(_Tp(-1) < _Tp(0)))>
{
};

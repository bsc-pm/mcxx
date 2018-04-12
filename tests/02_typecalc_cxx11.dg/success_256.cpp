/*
<testinfo>
test_generator="config/mercurium-cxx11"
test_nolink=yes
</testinfo>
*/

double bar(int i, int j) {}

template<typename _Tp>
class A {};

template<typename _Tp>
A<const _Tp> CA(const _Tp& __t) noexcept {
    return A<const _Tp>();
}

auto foo(int n) -> decltype(CA(bar))
{
   return CA(bar);
}

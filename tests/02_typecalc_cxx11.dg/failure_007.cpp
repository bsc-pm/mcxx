/*
<testinfo>
test_generator=config/mercurium-fe-only
test_CXXFLAGS="-std=c++11"
test_compile_fail=yes
</testinfo>
*/
template <typename T>
struct A
{
    int x;
};

template <typename T>
using B = typename A<T>::x;

B<int> c;

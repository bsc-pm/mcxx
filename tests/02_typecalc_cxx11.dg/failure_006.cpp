/*
<testinfo>
test_generator=config/mercurium-fe-only
test_CXXFLAGS="-std=c++11"
test_compile_fail=yes
</testinfo>
*/
template <typename T1, typename ...T2>
using A = int;

template <typename ...T3>
using B = A<T3...>; 

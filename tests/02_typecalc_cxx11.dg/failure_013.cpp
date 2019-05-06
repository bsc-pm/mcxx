/*
<testinfo>
test_generator="config/mercurium-cxx11"
test_compile_fail=yes
</testinfo>
*/

// Related report: http://www.open-std.org/jtc1/sc22/wg21/docs/cwg_active.html#1430
template<typename T> struct B_;

template<typename _BoundArg>
using B = B_<_BoundArg>;

template<typename... _BArgs>
using C_ = typename B< _BArgs... >::type;

template<typename... _Bound_args>
using C = C_< _Bound_args... >;

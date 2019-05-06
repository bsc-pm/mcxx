/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template<typename T> struct A;

template<typename T> struct B_;

template<typename _BoundArg>
using B = B_<_BoundArg>;

template<typename... _BArgs>
using C_ = typename A<  B<_BArgs>...  >::type;

template<typename... _Bound_args>
using C = C_< _Bound_args... >;

/*
<testinfo>
test_generator="config/mercurium"
test_CXXFLAGS="-std=c++11"
</testinfo>
*/

template <int ...N>
struct A { };

A<1, 2, 3, 4> a;

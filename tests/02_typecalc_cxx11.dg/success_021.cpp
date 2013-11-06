/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <int ...N>
struct A { };

A<1, 2, 3, 4> a;

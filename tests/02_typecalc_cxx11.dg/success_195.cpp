/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <int N> struct A { };

typedef A< 8 >= 2 > T1;
typedef A< 1 > T1;

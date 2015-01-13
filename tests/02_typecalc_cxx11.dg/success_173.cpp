/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/
template <typename T>
struct A;

template <typename ...P>
void f(A<int(P*)> ...m) { }

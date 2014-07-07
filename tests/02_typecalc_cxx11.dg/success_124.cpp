/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

template <class T> struct A {
operator T&();
// #1
operator T&&();
// #2
};
typedef int Fn();
A<Fn> a;
Fn& lf = a; // calls #1
Fn&& rf = a; // calls #2

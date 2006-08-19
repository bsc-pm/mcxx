int A;
template <template <class T> class Q = A<int> >
struct B { };

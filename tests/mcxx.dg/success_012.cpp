template <class Q = int, class S = float*>
struct A { };

A<> a;

int k;
template <int n = 3, int* p = (&k)>
struct B {
};

B<> b;

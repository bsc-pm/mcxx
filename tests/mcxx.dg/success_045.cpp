template <class T, class S = T*>
struct A;

typedef A<int> A_int;

template <class T>
struct M
{
    // This causes an improper instantiation of "A<T>"...
    typedef A<T> K;
};

template <class T, class S>
struct A
{
    typedef T C;
};

// ... causing this to fail
typedef A_int::C B;

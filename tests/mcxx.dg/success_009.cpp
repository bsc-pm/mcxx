template <class T, class Q = int>
struct A
{
    typedef Q P;
};

typedef A<int>::P p;
typedef int p;

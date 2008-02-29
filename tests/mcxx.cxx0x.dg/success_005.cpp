struct A
{
    int i;
};

typedef decltype(A::i) T;
typedef int T;

typedef decltype((A::i)) Q;
typedef int &Q;

struct A
{
    int i;
} *pA;

typedef decltype(pA->a) T;
typedef int T;

typedef decltype((pA->)) Q;
typedef int &Q;

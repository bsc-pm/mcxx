struct A
{
    int a;
} *pA;

typedef decltype(pA->a) T;
typedef int T;

typedef decltype((pA->a)) Q;
typedef int &Q;

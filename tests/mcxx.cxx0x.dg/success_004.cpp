int i;

typedef decltype(i) T;
typedef int T;

typedef decltype((i)) Q;
typedef int &Q;

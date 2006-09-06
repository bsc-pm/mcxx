template <int a, int b = a + 1>
struct A
{
    typedef int v1[a];
    typedef int v2[b];
	typedef int T;
};

A<10>::T t;

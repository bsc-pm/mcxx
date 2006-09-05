template <typename T>
struct A
{
};

template <typename T, 
    template<typename Q> class V = A >
struct M
{
	typedef typename V<int> T;
};

void f()
{
	M<int>::T k;
}

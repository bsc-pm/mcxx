template <typename T>
struct A
{
};

template <typename T, 
    template<typename Q> class V = A >
struct M
{
	typedef V<int> F;
};

void f()
{
	M<int>::F k;
}

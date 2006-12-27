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

M<int>::F k;

template <class _T>
struct A
{
	enum { __value = false };
	typedef _T T1;
};

template <>
struct A<int>
{
	enum { __value = true };

	typedef int *T1;
};

template<typename _T, typename _S = typename A<_T>::T1, bool _B = A<_T>::__value>
struct M
{
	typedef _S S;
};

M<int>::S m1;

template <typename _T>
struct size_is_10
{
	enum
	{
		__value = (sizeof(_T) == 10)
	};
};

template <typename _T, 
		 bool b = size_is_10<_T>::__value >
struct A
{
	typedef _T T;
};

void f()
{
	A<int>::T b;
}

template <typename T>
struct A
{
	typedef T P;
};

template <typename T, 
    template<typename Q> class V = A>
struct M
{
	typedef typename V<T>::P K;
};

M<int>::K k;
M<int> m;

template <typename _T>
struct A
{
	typedef _T T;
	typedef _T *P;

	const P* f();
};

template <typename _T>
const typename A<_T>::P* A<_T>::f()
{
}

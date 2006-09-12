template <class _T>
struct A
{
  typename _T::T f(int n);
  typename _T::T2 f(float n);

  typedef typename _T::T2 T3;
};

template <class _T>
typename _T::T A<_T>::f(int n)
{
}

template <class _T>
typename _T::T2 A<_T>::f(float n)
{
}

struct B
{
  typedef int T;
  typedef float T2;
};

A<B>::T3 t;

template <typename _T>
struct A
{
};

template <typename _T>
struct B
{
  typedef typename A<_T>::T T;

  T f();

  T f2();
};

template <typename _T>
typename B<_T>::T B<_T>::f()
{
}

template <typename _T>
typename A<_T>::T B<_T>::f2()
{
}

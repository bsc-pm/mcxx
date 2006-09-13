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

template <typename _S>
typename B<_S>::T B<_S>::f()
{
}

template <typename _S>
typename A<_S>::T B<_S>::f2()
{
}

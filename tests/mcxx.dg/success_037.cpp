template <class _T>
struct A
{
  typedef typename _T::T T;
};

template <class _T, class _S = typename A<_T>::T >
struct B
{
  typedef typename _S::T T;
};

struct C
{
   typedef 
   struct D
   {
     typedef float T;
   } T;
};

B<C>::T t;

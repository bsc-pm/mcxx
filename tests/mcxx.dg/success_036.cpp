template <class _T>
struct A
{
};

template <class _T>
struct B : A<_T>
{
	typedef _T T;
};

template <class _T>
struct C
{
  A<_T*> b;
};

A<int*>* t;

template <class _T>
struct A { 
  typedef _T *T2;
};

template <class _T>
struct C { 
	typedef _T T;
};

template <class _T>
struct B
{
	typedef A<_T> T1;
	typedef typename T1::T2 T3;

	typedef _T T;

	C<T3> f() { }
};

// Inst

void g()
{
  B<int>::T d;
  B<int> c;
  c.f();
}

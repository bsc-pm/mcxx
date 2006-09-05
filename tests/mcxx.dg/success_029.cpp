template <typename T>
struct A 
{
	typedef T M;
};

template <typename T>
struct B 
{ 
};

template <typename T>
struct B<A<T> > 
{
	typedef A<T>::M K;
};

B<A<int> >::K b;

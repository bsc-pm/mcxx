namespace C
{
	template <typename S>
	struct A
	{
	};
}

template <template <typename S> class V>
struct B
{
	typedef V<int> T;
};

B<C::A>::T t;
B<C::A> b;

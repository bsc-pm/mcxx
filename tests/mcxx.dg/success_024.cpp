template <typename T, 
		 typename S = T*,
		 typename Q = S**>
struct M;

template <typename A, 
		 typename B, 
		 typename C>
struct M
{
	typedef A a;
	typedef B b;
	typedef C c;
};

void f()
{
	M<int>::a m;
}

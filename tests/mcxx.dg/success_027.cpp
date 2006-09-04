template <typename T>
struct A
{
};

template <typename T, 
    typename S = A<T> >
struct M
{
	typedef S K;
};

M<int> m;
M<int> k;

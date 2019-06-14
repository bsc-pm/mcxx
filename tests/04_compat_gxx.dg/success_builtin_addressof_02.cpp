/*
<testinfo>
test_generator=config/mercurium-fe-only
test_CXXFLAGS="-std=c++11"
</testinfo>
*/
template <typename T>
struct A
{
	static T var;
};

template <typename T, typename W = decltype(__builtin_addressof(T::var))>
struct B
{
  using type = W;
};

using Check = float*;
using Check = B<A<float>>::type;

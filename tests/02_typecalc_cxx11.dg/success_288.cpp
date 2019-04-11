/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/
template <typename T>
struct sizes
{
  constexpr static unsigned int N = 100;
};

template <typename T>
struct array
{
    T v[sizes<T>::N];
};

void foo()
{
    array<int> a;
}

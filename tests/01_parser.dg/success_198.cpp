/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename _T>
struct my_allocator;

template <typename _T, typename _Alloca = my_allocator<_T> >
struct my_vector
{
};

template <typename _S>
void f(int k, const my_vector<_S>& def = my_vector<_S>())
{
}

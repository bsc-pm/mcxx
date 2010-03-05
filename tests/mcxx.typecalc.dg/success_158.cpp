/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
template <typename _Q>
struct A
{
    template <typename _T>
    A(_T)
    {
    }
};

void f()
{
    A<int> a(3);
}

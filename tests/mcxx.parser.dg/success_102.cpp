/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
template <int _N>
struct B
{
};

struct A
{
    template <int _N>
    B<_N> foo(void);
};

template<>
B<0> A::foo(void)
{
}

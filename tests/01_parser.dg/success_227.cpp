/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename _FooT, typename _BarT>
struct A
{
    typedef _FooT foo_type;
    typedef _BarT bar_type;

    typedef A<foo_type, bar_type> myself;

    void g();
};

template <typename _FooT, typename _BarT>
void A<_FooT, _BarT>::g()
{
}

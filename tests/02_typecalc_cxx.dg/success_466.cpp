/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T>
struct A
{
    struct const_iterator;

    const_iterator foo(int x) const
    {
        return const_iterator(*this, x);
    };

    struct const_iterator
    {
        const_iterator(const A&, int);
    };
};

void g(const A<int>& a)
{
    a.foo(4);
}

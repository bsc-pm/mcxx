/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
struct my_allocator
{
};

template <typename T, typename Alloc = my_allocator>
struct my_vector
{
    my_vector(unsigned n, T default_value = T(), const Alloc& alloc = Alloc());

    template <typename _Iterator>
    my_vector(_Iterator begin, _Iterator end, const Alloc& alloc = Alloc())
    {
        typename _Iterator::Foo x;
    }
};

void f()
{
    my_vector<double> v(0.0);
}

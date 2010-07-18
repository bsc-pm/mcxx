/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
struct B
{
    template <typename _T>
    friend struct A;
};

template <typename _T>
struct A
{
    // void operator()(int) { }
    void foo(void) { }
};

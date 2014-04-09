/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T>
struct A
{
    struct dummy
    {
        void nonnull();
    };
    typedef void (dummy::*safe_bool)();
};

void f()
{
    A<int>::dummy b;
    A<int>::safe_bool c;

    (b.*c)();
}

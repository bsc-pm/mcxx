/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
namespace A
{
    template <typename _T>
        struct B { };
}

extern template A::B<int>;

/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
template <typename _T>
struct B { };

struct A
{
    friend struct AClass Aclass(B<A>);
};

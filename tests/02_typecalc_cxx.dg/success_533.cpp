/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
template <typename T>
struct A { };

struct foo
{
    friend struct A<foo>;
};


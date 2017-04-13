/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
template < bool b>
struct C
{ };

struct Base { };

template < typename T>
struct B
{
    struct C : Base
    {};
};

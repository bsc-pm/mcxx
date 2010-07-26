/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
struct A1
{
    friend struct B1;
};

struct B1 { };

struct A2
{
    template <typename _T>
    friend struct B2;
};

template <typename _T>
struct B2 { };

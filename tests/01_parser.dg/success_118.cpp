/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
template <int N>
struct A
{
    union data
    {
        char c[N];
    } data_;
};

A<3> a;

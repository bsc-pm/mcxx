/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

namespace foo
{
    template <typename T>
        struct vector { };
}

template <typename T>
struct A { };

namespace foo
{
    int x;
}

template struct A<foo::vector<int> >;

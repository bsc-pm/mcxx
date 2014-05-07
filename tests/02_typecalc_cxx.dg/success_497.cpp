/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

namespace foo
{
    int x;
}

namespace foo2
{
    int x;
}

void bar()
{
    int foo;
    typedef int foo2;
    foo::x = 3;
    foo2::x = 3;
}

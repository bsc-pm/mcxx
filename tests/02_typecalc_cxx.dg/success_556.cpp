/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

enum E { V1, V2 };

static const E def = V2;

template <E e>
struct A
{
    typedef int Type;
};

A<def>::Type a;

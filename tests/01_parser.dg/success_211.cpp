/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

namespace M {
    // Incomplete
    struct foo;
    // Complete
    struct foo2 { };
}

namespace B{
    namespace M_
    {
        using M::foo;
        using M::foo2;
    }
}

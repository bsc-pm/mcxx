/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

namespace A {
    namespace D
    {
    }
}

namespace B
{
    namespace C = A::D;
}

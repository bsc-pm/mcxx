/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
namespace S
{
    namespace T
    {
        namespace P
        {
            namespace
            {
                int x;
            }
        }
    }
}

namespace {
    int y;
}
namespace S
{
    namespace P
    {
        using S::T::P::x;
    }
}

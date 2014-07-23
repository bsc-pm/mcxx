/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

namespace N
{
    struct C
    {
        enum struct E1
        {
            A1 = 1
        };
        enum E2
        {
            A2 = 1
        };
    };
}

void f()
{
    {
        N::C::E1 x;
        x = N :: C :: E1 :: A1;
    }
    {
        N::C::E2 x;
        x = N :: C :: E2 :: A2;
    }
}

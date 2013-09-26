/*
<testinfo>
test_generator="config/mercurium cxx11"
test_CXXFLAGS="-std=c++11"
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
    int x;
    x = N :: C :: E1 :: A1;
    x = N :: C :: E2 :: A2;
}

struct B
{
    static int m;
};

namespace A
{
    typedef int T;
};

using namespace A;

T B::m = 3;

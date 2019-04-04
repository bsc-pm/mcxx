/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/
struct C
{};

struct D
{
    D(int x, int y) {}
};

struct E
{
    C c {};
    D d {1, 2};
};

/*
<testinfo>
test_generator="config/mercurium-cxx11"
test_nolink=yes
</testinfo>
*/

struct A
{
    int x;
    constexpr A() : A(42) { }
    constexpr A(int x) : x(x) { }
};


static_assert(A().x == 42, "");

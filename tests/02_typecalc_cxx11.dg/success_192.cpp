/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

struct A
{
    int x;
};

struct B
{
    A a;

    constexpr B() : a({1}) { }
};


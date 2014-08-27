/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

struct A
{
    void g()
    {
        typedef decltype(this) test_type;
        typedef A* test_type;
    }
};

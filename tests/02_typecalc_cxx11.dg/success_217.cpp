/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/
struct S
{
    S() : s(__func__)
    {
    } // ok
    const char *s;
};

/*
<testinfo>
test_generator=config/mercurium-ompss-2
test_compile_fail=yes
test_nolink=yes
</testinfo>
*/

void foo()
{
    int x;

    #pragma oss task weakreduction(+: x) reduction(+: x)
    {
    }

    #pragma oss taskwait
}

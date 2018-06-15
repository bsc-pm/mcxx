/*
<testinfo>
test_generator=config/mercurium-ompss-2
test_compile_fail=yes
test_nolink=yes
</testinfo>
*/

void foo()
{
    int x = 0;

    #pragma oss task reduction(+: x)
    {
        #pragma oss release reduction(+: x)
        {
        }
    }

    #pragma oss task weakreduction(+: x)
    {
        #pragma oss release weakreduction(+: x)
        {
        }
    }

    #pragma oss taskwait
}

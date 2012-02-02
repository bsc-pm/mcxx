/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

void foo()
{
#if defined(__i386__) || defined(__x86_64__)
    ({
        asm ("nop");
    });
#endif
}

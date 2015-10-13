/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

typedef __attribute__((vector_size(16))) unsigned int v4sui;

v4sui f(v4sui ua, v4sui ub)
{
#if __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 8)
    ub = ua == ua;
#endif

    return ub;
}


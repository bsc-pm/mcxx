/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

typedef int int32_t;

static int opal_atomic_cmpset_32( volatile int32_t *addr,
                                        int32_t oldval, int32_t newval)
{
    *addr = 3;
    oldval = 4;
    newval = 5;
    int ret = 3;

   return (int)ret;
}

static
int opal_atomic_cmpset_32(volatile int32_t *addr, int32_t oldval,
                          int32_t newval);

static
int opal_atomic_cmpset_32(volatile int32_t *addr, int32_t oldval,
                              int32_t newval);


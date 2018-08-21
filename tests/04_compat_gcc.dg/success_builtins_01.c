/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
#if ((__GNUC__ > 4) \
        || (( __GNUC__ == 4) && __GNUC_MINOR__ >= 8))

#include<stdint.h>

void foo() {
    uint16_t var = 0xAABB;
    uint16_t res = __builtin_bswap16(var);
}

#endif

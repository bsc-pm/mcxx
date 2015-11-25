/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
#include <stdint.h>

void foo();

void g()
{
    intptr_t t;
    t = (intptr_t)(void(*)())foo;
}

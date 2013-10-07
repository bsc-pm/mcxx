/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

#include <stdarg.h>

void f(int a, ...);

void g(int a, ...)
{
   va_list v;
   va_start(v, a);

   f(a, v);
}

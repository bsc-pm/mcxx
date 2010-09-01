/*
<testinfo>
test_generator=config/mercurium-omp

test_compile_fail_nanox_plain=yes
test_compile_faulty_nanox_plain=yes
</testinfo>
*/

#include <stdio.h>
#include <stdlib.h>

#define N 100

struct myInt {
   int x;
};

#pragma omp declare reduction(+:struct myInt: _out.x += _in.x)

int main (int argc, char **argv)
{
   int i,s=0;
   int a[N];
   struct myInt x = {0};

   for ( i = 0; i < N ; i++ ) {
       a[i] = i;
       s += i;
   }

   #pragma omp parallel for reduction(+:x)
   for ( i = 0; i < N ; i++ )
   {
        x.x += a[i];
   }

   if ( x.x != s ) abort();
   return 0;
}

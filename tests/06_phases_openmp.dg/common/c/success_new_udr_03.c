/*
<testinfo>
test_generator=config/mercurium-omp

test_compile_fail_nanox_plain=yes
test_compile_faulty_nanox_plain=yes
</testinfo>
*/

#include <stdio.h>
#include <stdlib.h>

typedef int Myint;

#pragma omp declare reduction(min:Myint: _out = _out > _in ? _in : _out ) identity(2147483647)

#define N 100

int main (int argc, char **argv)
{
   int i,x = N+1;
   int a[N];

   for ( i = 0; i < N ; i++ ) a[i] = i;

   #pragma omp parallel for reduction(min:x)
   for ( i = 0; i < N ; i++ )
   {
        x = a[i] < x ? a[i] : x;
   }

   if ( x != 0 ) abort();
   return 0;
}

/*
<testinfo>
test_generator="config/mercurium-ompss"
test_compile_fail_nanos6_mercurium=yes
test_compile_fail_nanos6_imcc=yes
</testinfo>
*/
#include <omp.h>
#include <stdio.h>

// This is to ensure the high priority task is not executed twice or more
int done = 0;

#pragma omp task /*output(*depth)*/ priority(0)
void normal_task (int * var)
{
   int i;
   ++( *var );
}

#pragma omp task /*input(*depth)*/ priority(10000)
void high_task(int * var)
{
   int i;
   #pragma omp critical( my_lock )
   {
      if( done == 0 )
       *var = 0;
      done = 1;
   }
}

#define NUM_ITERS 100


int main ()
{
   int A = 0;
   int i, j;
   int check = 1;

   nanos_stop_scheduler();
   nanos_wait_until_threads_paused();

   for (j=0; j < NUM_ITERS; j++) {
      normal_task(&A);
   }

   for (i = 0; i < omp_get_num_threads(); i++) {
      high_task(&A);
   }

   nanos_start_scheduler();
   nanos_wait_until_threads_unpaused();

#pragma omp taskwait
   check = ( A != 0 );

   if ( !check ) {
       fprintf(stderr, "FAIL %d\n", A);
      return 1;
   }
   return 0;
}



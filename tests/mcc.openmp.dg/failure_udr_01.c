#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

typedef int myint;

#pragma omp declare reduction type(myint) operator(+) identity(1)

int main ()
{
myint total = 0;

#pragma omp parallel reduction(+:total)
{
	total += omp_get_thread_num();
}

printf("total = %d\n",total);

}

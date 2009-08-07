#include <stdio.h>
#include <omp.h>

typedef struct {
	int value;
} myint;

#pragma omp declare reduction type(myint) operator(+,-) identity({0})


int main ()
{
	myint total = {0};

	#pragma omp parallel reduction(+:total)
	{
		total.value = omp_get_thread_num();
	}

	printf("total = %d\n",total.value);
}

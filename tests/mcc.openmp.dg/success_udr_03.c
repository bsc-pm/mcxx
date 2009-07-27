#include <stdio.h>
#include <omp.h>

typedef struct {
	int value;
} myint;

void myint_add (myint *a, myint b)
{
	a->value += b.value;
}

#pragma omp declare reduction type(myint) operator(myint_add) identity({0})

int main (int argc, char *argv[])
{
	myint total = {0};

	#pragma omp parallel reduction(myint_add:total)
	{
		myint num = {omp_get_thread_num()};
		myint_add(&total,num);
	}

	printf("total = %d\n",total.value);
}

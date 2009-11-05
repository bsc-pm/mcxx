#include <stdio.h>
#include <omp.h>

typedef struct {
	int value;
} myint;

void myint_add (myint *a, myint *b)
{
	b->value += a->value;
}

#pragma omp declare reduction(myint_add:myint) identity({0}) order(right)

int main (int argc, char *argv[])
{
	myint total = {0};

	#pragma omp parallel reduction(myint_add:total)
	{
		myint num = {omp_get_thread_num()};
		myint_add(&num, &total);
	}

	printf("total = %d\n",total.value);
}

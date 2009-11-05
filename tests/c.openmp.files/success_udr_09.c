#include <stdio.h>
#include <omp.h>

typedef struct {
	int value;
} myint;

myint myint_add (myint *a, myint b)
{
    myint res = *a;
    res.value += b.value;

    return res;
}

#pragma omp declare reduction(myint_add:myint) identity({0})

int main (int argc, char *argv[])
{
	myint total = {0};

	#pragma omp parallel reduction(myint_add:total)
	{
		myint num = {omp_get_thread_num()};
		total = myint_add(&total, num);
	}

	printf("total = %d\n",total.value);
}

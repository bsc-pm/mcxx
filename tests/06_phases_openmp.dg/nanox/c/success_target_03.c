/*
<testinfo>
test_generator=config/mercurium-nanox
</testinfo>
*/

const int M = 20;

int main (int argc, char *argv[])
{
int *a;

#pragma omp target device(smp_numa) copy_in(a[0:M-1])
#pragma omp task shared(a)
{
}
}


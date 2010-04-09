/*
<testinfo>
test_generator=config/mercurium-nanox
</testinfo>
*/

const int M = 20;

int main (int argc, char *argv[])
{
int *a;

#pragma omp task inout([M] a)
{
}
}


/*
<testinfo>
test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
</testinfo>
*/

#pragma omp task inout(*var) node(*var)
void foo(int* var)
{
    (void)var;
}

#pragma omp task inout(*var) stream(*var2)
void foo2(int* var2)
{
    (void)var2;
}


int main()
{
    int a = 1;
    foo(&a);

    foo2(&a);

    #pragma omp task node(0)
    {
    }

	#pragma omp task stream(1)
    {
    }

    #pragma omp taskwait
}

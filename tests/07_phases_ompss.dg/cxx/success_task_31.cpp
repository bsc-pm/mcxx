/*
<testinfo>
test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
</testinfo>
*/

#pragma omp task inout(*var) cost(*var)
void foo(int* var)
{
    var++;
}

int main()
{
    int a;
    foo(&a);

    #pragma omp task cost(0)
    {
    }

    #pragma omp taskwait
}

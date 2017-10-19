/*
<testinfo>
test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
test_nolink=yes
</testinfo>
*/

// This test was designed to stress our transformation of the final clause.
// There is no need to execute it
int main()
{
    int x = 0;
    #pragma omp task inout(x)
    {
        x++;
        #pragma omp taskwait inout(x)
    }
    #pragma omp taskwait
}

/*
<testinfo>
test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
test_nolink=yes
</testinfo>
*/
int foo(int n) {
    int v[n];
    int v2[10];
    #pragma omp task inout(v, v2)
    {}
}

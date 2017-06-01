/*
<testinfo>
test_generator="config/mercurium-ompss"
test_nolink=yes
</testinfo>
*/
int foo(int *v, int m, int n) {
    #pragma omp task inout( ((int (*)[n])v)[0;m])
    {}
}

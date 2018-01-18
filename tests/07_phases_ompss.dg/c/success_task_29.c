/*
<testinfo>
test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
test_nolink=yes
</testinfo>
*/
typedef struct A {
    int n;
} A;


void foo(int N, int *p) {
    A var = { N };
    #pragma omp task inout([var.n]p)
    {}
}

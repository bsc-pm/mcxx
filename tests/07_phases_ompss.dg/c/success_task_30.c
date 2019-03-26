/*
<testinfo>
test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
test_nolink=yes
</testinfo>
*/
enum { N = 10 };

int main(int argc, char *argv[])
{
    int v[N];
    int *p = v;

    #pragma omp task inout([N]p)
    {}

    return 0;
}

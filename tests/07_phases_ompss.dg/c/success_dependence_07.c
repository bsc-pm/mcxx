/*
<testinfo>
test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
test_nolink=yes
</testinfo>
*/

void foo(n)
{
    int (*A)[n];

    #pragma oss task in(A)
    {
    }

    #pragma oss taskwait
}

/*
<testinfo>
test_generator=config/mercurium-ompss-2
test_nolink=yes
</testinfo>
*/

void bar(int* x)
{
    #pragma oss task in([1](char*)x)
    *x = ~(*x); // x is an int* at this point!

    #pragma oss taskwait
}

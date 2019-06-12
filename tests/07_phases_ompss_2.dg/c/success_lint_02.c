/*
<testinfo>
test_generator=config/mercurium-ompss-2
test_nolink=yes
</testinfo>
*/
int main()
{
    int *p = 0;
    int N = 100;
    #pragma oss lint alloc(p[0;N])
    {
        //p = malloc(N*sizeof(*p));
    }

    #pragma oss lint free(p)
    {
        // free(p);
    }
}

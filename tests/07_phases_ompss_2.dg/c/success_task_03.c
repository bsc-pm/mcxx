/*
<testinfo>
test_generator=config/mercurium-ompss-2
</testinfo>
*/
void foo(int n)
{
    int i;
    int v[10];
    int w[n];

    #pragma oss task
    {
        #pragma oss task
        {
            for (i = 0; i < 10; ++i) v[i] = 0;
            for (i = 0; i < n; ++i)  w[i] = 0;
        }
    }
    #pragma oss taskwait
}

int main()
{
    foo(100);
}

/*
<testinfo>
test_generator=config/mercurium-ompss-2
test_nolink=yes
</testinfo>
*/
int main()
{
    int x;
    #pragma oss task inout(x) verified
    {
    }

    #pragma oss task inout(x) verified(x < 5)
    {
    }

    #pragma oss task inout(x)
    {
    }

    #pragma oss taskwait
}

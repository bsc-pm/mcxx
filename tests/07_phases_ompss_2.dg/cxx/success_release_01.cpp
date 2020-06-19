/*
<testinfo>
test_generator="config/mercurium-ompss-2"
test_ignore=yes
test_ignore_reason="feature not supported by nanos6"
</testinfo>
*/
#include<assert.h>

int main()
{
    int x = 1;
    #pragma oss task inout(x)
    {
        x++;
        #pragma oss release out(x)
        assert(x == 2);
    }
    #pragma oss task in(x)
    assert(x == 2);

    #pragma oss taskwait
}

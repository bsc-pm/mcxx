/*
<testinfo>
test_generator="config/mercurium run"
</testinfo>
*/

#include <stdlib.h>

int main(int argc, char *argv[])
{
    int x;
    if (__alignof__(x) != __alignof__(int))
        abort();

    double y;
    if (__alignof__(y) != __alignof__(double))
        abort();

    return 0;
}

/*
<testinfo>
test_generator="config/mercurium run"
</testinfo>
*/

#include <stdlib.h>

int main(int argc, char* argv[])
{
#ifdef __x86_64__
    __attribute__((vector_size(16))) float a;

    if (sizeof(a) != 16)
        abort();

    if (__alignof__(a) != 16)
        abort();
#endif

    return 0;
}

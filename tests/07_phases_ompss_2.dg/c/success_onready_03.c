/*
<testinfo>
test_generator="config/mercurium-ompss-2"
</testinfo>
*/

#include <assert.h>

void bar(int *n) {
    *n = 77;
}

int main() {
    int n = 4;
    int i;
    #pragma oss task for onready(bar(&n)) shared(n)
    for (i = 0; i < 10; ++i)
    {}
    #pragma oss taskwait
    assert(n == 77);
}

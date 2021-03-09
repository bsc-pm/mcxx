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
    #pragma oss task onready(bar(&n)) shared(n)
    {}
    #pragma oss taskwait
    assert(n == 77);
}

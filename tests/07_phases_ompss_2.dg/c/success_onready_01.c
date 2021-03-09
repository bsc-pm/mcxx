/*
<testinfo>
test_generator="config/mercurium-ompss-2"
</testinfo>
*/

#include <assert.h>

void bar(int *n) {
    *n = 77;
}

#pragma oss task onready(bar(n))
void foo(int *n) {  }

int main() {
    int n = 4;
    foo(&n);
    #pragma oss taskwait
    assert(n == 77);
}

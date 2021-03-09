/*
<testinfo>
test_generator="config/mercurium-ompss-2"
test_exec_fail=yes
</testinfo>
*/

#include <stdlib.h>

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
    if (n != 77)
        exit(1);
}

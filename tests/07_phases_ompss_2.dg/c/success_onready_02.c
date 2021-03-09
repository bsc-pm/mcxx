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
    #pragma oss task onready(bar(&n)) shared(n)
    {}
    #pragma oss taskwait
    if (n != 77)
        exit(1);
}

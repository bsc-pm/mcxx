/*
<testinfo>
test_generator="config/mercurium-ompss-2"
test_exec_fail=yes
</testinfo>
*/

void bar(int *n) {
    *n = 77;
}

#pragma oss task onready(bar(n))
void foo(int *n) {  }

int main() {
    int n = 4;
    foo(&n);
    #pragma oss taskwait
    if (n != 77)
        return 1;
    return 0;
}

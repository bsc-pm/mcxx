/*
<testinfo>
test_generator="config/mercurium-ompss-2"
</testinfo>
*/

void bar(int *n) {
    *n = 77;
}

int main() {
    int n = 4;
    #pragma oss task onready(bar(&n)) shared(n)
    {}
    #pragma oss taskwait
    if (n != 77)
        return 1;
    return 0;
}

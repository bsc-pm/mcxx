/*
<testinfo>
test_generator="config/mercurium-nanos6-regions"
</testinfo>
*/

#include <assert.h>

#pragma oss task reduction(+: [1]p)
void bar(int *p) {
    (*p)++;
}

#pragma oss task weakreduction(+: [10]array)
void foo(int *array) {
    for (int i = 0; i < 10; ++i) {
        bar(&array[i]);
    }
}

int main() {
    int array[10];
    for (int i = 0; i < 10; ++i) {
        array[i] = 0;
    }

    foo(array);

    #pragma oss taskwait

    for (int i = 0; i < 10; ++i) {
        assert(array[i] == 1);
    }
}

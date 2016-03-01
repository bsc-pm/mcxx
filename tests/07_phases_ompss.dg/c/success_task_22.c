/*
<testinfo>
test_generator="config/mercurium-ompss"
test_compile_fail_nanos6_mercurium=yes
test_compile_fail_nanos6_imcc=yes
</testinfo>
*/
#include <assert.h>

int i_global = 0;
void f(int i_param, int check)
{
    int i_local = 0;
    int x = i_param;

#pragma omp task
    void h(void)
    {
        i_local++;
        i_param++;
        i_global++;
    }

    h();
#pragma omp taskwait

    assert(i_local == 1);
    assert(x + 1 == i_param);
    assert(check + 1 == i_global);
}

int main(int argc, char *argv[])
{
    f(1, 0);
    f(2, 1);

    return 0;
}

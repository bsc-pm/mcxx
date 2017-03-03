/*
<testinfo>
test_generator="config/mercurium-ompss"
</testinfo>
*/

/*

 Description: The goal of this example is to show how the depend clause applied
 to the taskwait construct can be used to wait for the results of an specific
 task.

 This example is the same as the previous one but with one change: the taskwait
 construct defines an inout dependence over 'x' rather than an input dependence.
 The effect of this change is that the region is suspended until both tasks
 complete execution.

*/

#include<assert.h>

void foo()
{
    int x = 0, y = 2;

    #pragma omp task depend(inout: x) shared(x)
    x++;

    #pragma omp task depend(in: x) depend(inout: y) shared(x, y)
    y -= x;

    #pragma omp taskwait depend(inout: x)
    assert(x == 1);
    assert(x == y);
}

int main(int argc, char*argv)
{
    foo();
    return 0;
}

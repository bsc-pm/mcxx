/*
<testinfo>
test_generator="config/mercurium-ompss"
</testinfo>
*/

/*

 Description: The goal of this example is to show how the depend clause applied
 to the taskwait construct can be used to wait for the results of an specific
 task.

 In more detail, in this example we have two tasks whose execution is
 serialized via dependences: the first one produces the value of 'x' whereas
 the second one consumes 'x' and produces the value of 'y'.  In addition to
 this, we have a taskwait construct annotated with an input dependence over
 'x', which enforces that the execution of the region is suspended until the
 first task complete execution. Note that this construct does not introduce any
 restriction on the execution of the second task.

 Finally, we have to guarantee that the second task is executed before exiting
 the 'foo' function, so we have a taskwait construct that waits for all the
 task.

*/

#include<assert.h>

void foo()
{
    int x = 0, y = 2;

    #pragma omp task depend(inout: x) shared(x)
    x++;

    #pragma omp task depend(in: x) depend(inout: y) shared(x, y)
    y -= x;

    #pragma omp taskwait depend(in: x)
    assert(x == 1);

    // Potentially race condition, note that the second task may not be
    // executed at this point!
    // assert(x == y);

    #pragma omp taskwait
    assert(x == y);
}

int main(int argc, char*argv)
{
    foo();
    return 0;
}

/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

#include <stdlib.h>

template <typename T>
void f(T& x)
{
   ( typename T::iterator(x, x) );
}

int z = 0;

struct A
{
    struct iterator
    {
        iterator (A& a, A& b)
        {
            z = 42;
        }
    };
};

int main(int argc, char *argv[])
{
    A a;
    f(a);

    if (z != 42) abort();

    return 0;
}

/*
<testinfo>
test_generator="config/mercurium run"
</testinfo>
*/

#include <stdlib.h>

template <int N>
void f()
{
    int x1 = bool(N);
    int x2 = (bool)N;

    if (x1 != 0 && x1 != 1)
        abort();

    if (x2 != 0 && x2 != 1)
        abort();
}

int main(int argc, char *argv[])
{
    f<0>();
    f<1>();
    f<2>();
    f<-1>();
    f<-2>();

    return 0;
}

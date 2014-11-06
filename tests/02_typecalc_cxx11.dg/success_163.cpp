/*
<testinfo>
test_generator="config/mercurium-cxx11 run"
</testinfo>
*/
#include <assert.h>

int main(int, char**)
{
    int z;
    auto f1 = [](int x) { return x + 1; };
    z = f1(1);
    assert(z == 2);

    int y = 1;
    auto f2 = [y](int x) { return x + y; };
    z = f2(1);
    assert(z == 2);
}

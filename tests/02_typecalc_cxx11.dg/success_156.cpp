/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

template <typename T>
int* f(T); // #1

template <typename T, typename ...S>
float* f(T, S...); // #2

void g()
{
    // See DR1395. Technically this call is ambiguous
    // per the Standard (last example in 14.5.6.2p5)

    int *pi = f(1); // OK. Chooses #1
}

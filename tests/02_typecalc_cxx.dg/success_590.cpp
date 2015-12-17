/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
#include <assert.h>

typedef __typeof__(sizeof(0)) size_t;

template<typename T, size_t Size> char (&ArrayLengthHelperFunction(T (&)[Size]))[Size];

int arr1[] = { 1, 2, 3, 4 };
const int arr2[] = { 1, 2, 3, 4 };

int main(int argc, char** argv)
{
    size_t s;
    s = sizeof(ArrayLengthHelperFunction(arr1)); // OK
    assert(s == 4);

    s = sizeof(ArrayLengthHelperFunction(arr2)); // ERROR
    assert(s == 4);

    return 0;
}

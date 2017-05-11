/*
<testinfo>
test_generator="config/mercurium-hlt run"
</testinfo>
*/

#include <iostream>
#include <cassert>

int main()
{
    // cppreference.com asserts:
    //      "C++ Standard guarantees that 1 == sizeof(char) <= sizeof(short) <= sizeof(int) <= sizeof(long) <= sizeof(long long)."

    // result (effective) iteration space (product) will be > 2^(8-1) - 1, in fact it is exactly 2^7

    char lb_i = 0;
    char lb_j = 0;
    char lb_k = 0;

    char ub_i = 4;
    char ub_j = 4;
    char ub_k = 8;

    char s_i = 1;
    char s_j = 1;
    char s_k = 1;

    unsigned int result = 0;

    // Collapsed loops

    #pragma hlt collapse(3)
    for (char i = lb_i; i < ub_i; i += s_i)
    for (char j = lb_j; j < ub_j; j += s_j)
    for (char k = lb_k; k < ub_k; k += s_k)
    {
        result++;
    }

    assert(result == (1 << 7));
    std::cout << "result: " << result << std::endl;
}

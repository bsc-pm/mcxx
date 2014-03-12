/*
<testinfo>
test_generator="config/mercurium run"
</testinfo>
*/

#include <stdlib.h>

struct small
{
    char c;
};

struct big
{
    char c[2];
};

namespace A
{
    enum E { V } ;
    small f(E);
}

big f(int);

template <typename T>
struct B
{
    static const int N = sizeof(f(*(T*)0));   // Calls ::A::f(E)
    static const int M = sizeof((f)(*(T*)0)); // Calls ::f(int)
};

template <int N, int M>
struct Different { typedef int Type; };

template <int N>
struct Different<N, N> { };

const int N = B<A::E>::N;
const int M = B<A::E>::M;

Different<N, M>::Type t;

int main(int argc, char* argv[])
{
    if (N == M)
        abort();
    return 0;
}

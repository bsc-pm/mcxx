/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T>
struct ComputeAlign
{
    static const unsigned long value = __alignof__(T);
};

template <int>
struct Verifier { };

struct A
{
    int a;
    int b;
    int c;
};

template <>
struct Verifier<__alignof__(A)> { typedef int K; };

void f()
{
    Verifier<ComputeAlign<A>::value>::K k;
}

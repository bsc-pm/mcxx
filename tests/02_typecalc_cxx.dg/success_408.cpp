/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T>
struct A
{
    operator T*();
};

template <typename T>
struct B
{
    operator T*();
    void operator [](float*); // Should not be called
};

void f()
{
    // --
    A<int> a;

    *(a + 1) = 3; // -> a[0]
    *(1 + a) = 3; // -> 0[a]

    a[1] = 3; // -> *(a + 0)
    1[a] = 3; // -> *(0 + a)

    // --
    B<int> b;

    *(b + 1) = 3; // -> b[0]
    *(1 + b) = 3; // -> 0[b]

    b[1] = 3; // -> *(b + 0)
    1[b] = 3; // -> *(0 + b)
}


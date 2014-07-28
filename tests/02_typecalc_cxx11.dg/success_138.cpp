/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

struct A
{
    struct K { };
};

typedef int T;

struct B
{
    friend A;
    friend T;

    friend void foo();

    friend void bar()
    {
    }
};

template <typename T>
struct C
{
    friend typename T::K;
};

C<A> c;

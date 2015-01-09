/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T>
struct A
{
    struct Test;

    typedef typename Test::Moo Type;

    struct Test
    {
        typedef int Moo;
    };
};

typedef A<int>::Type T;
typedef int T;

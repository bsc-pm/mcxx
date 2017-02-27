/*
<testinfo>
test_generator=config/mercurium-fe-only
test_compile_fail=yes
</testinfo>
*/
namespace N
{
    template < typename T>
        struct C
        {
            typename T::K m;
        };

    typedef C<int> MyC;
}

template < char>
N::C<int> fun()
{
    return 1;
}

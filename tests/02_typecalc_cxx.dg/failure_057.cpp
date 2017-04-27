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


template < typename T, char ...c>
T kk() {}

template < char... c>
N::MyC fun()
{
    return kk< N::MyC, c... >();
}

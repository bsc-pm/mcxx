/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

struct A
{
    template<class _Tp>
        const A&
        operator=(const _Tp&) const
        { return *this; }
};
const A ignore{};

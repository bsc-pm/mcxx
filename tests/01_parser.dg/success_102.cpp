/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
template<typename _Q >
struct B
{
        template<typename _S >
        struct C
        {
        };
        C<_Q> c;
};
B<int> b;

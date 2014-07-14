/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
template <typename S1>
struct B1
{
    friend class C;
};

template <typename S1>
struct B2
{
     template <typename S2>
         friend class C;
};

/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
template <typename T>
class A
{
    template <typename S>
        class B
        {
            friend class A;
        };
};

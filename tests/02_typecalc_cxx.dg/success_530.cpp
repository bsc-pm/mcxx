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
            template <typename W>
            friend class C;
        };

    template <typename S>
        class C
        {
        };
};

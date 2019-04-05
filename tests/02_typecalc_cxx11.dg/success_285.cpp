/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/
struct C
{
    friend constexpr bool bar() noexcept
    {
        return true;
    }
};

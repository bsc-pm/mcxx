/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

template <typename T>
struct SizeOf
{
    constexpr static int value()
    {
        return sizeof(T);
    }
};

void f()
{
    static_assert(SizeOf<double>::value() == sizeof(double), "Invalid sizeof");
}

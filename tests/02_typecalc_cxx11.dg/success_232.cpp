/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/
struct A
{
    template < unsigned int n >
    constexpr float get_power(float x)
    {
        return x * get_power<n-1>(x);
    }

};

template <>
constexpr float A::get_power<0>(float x)
{
    return 1.0;
}

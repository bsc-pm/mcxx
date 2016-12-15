/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/
template <unsigned int n>
constexpr const float get_power(float x)
{
    return x * get_power<n-1>(x);

}

template <>
constexpr const float get_power<0>(float x)
{
    return 1.0;
}


int main()
{
    float res = get_power<0>(2.0);
}

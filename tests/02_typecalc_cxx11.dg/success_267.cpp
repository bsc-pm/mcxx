/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/
template<typename _B1, typename _B2>
struct AND
{
    static constexpr bool value = true;
};

template<typename _Tp, typename... _Args>
struct IsX
{};


template<typename... _Elements>
static constexpr bool are_X()
{
    return AND< IsX< _Elements, _Elements>... >::value;
    // the problem is how isX<_Elements, _Elements> ... is expanded. Note that
    // this expression should be expanded to: AND < isX<int, int>, isX<float, float> >::value
}

template < typename T1, typename T2, bool b = are_X<T1, T2>() >
void foo() {}

int main()
{
    foo<int, float>();
}


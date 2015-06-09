/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

template<class _T1, class _T2>
struct my_pair
{
    typedef _T1 first_type;
    typedef _T2 second_type;

    _T1 first;
    _T2 second;

    constexpr my_pair()
        : first(), second() { }

    constexpr my_pair(_T1 first, _T2 second)
        : first(first), second(second) { }

    constexpr my_pair(const my_pair&) = default;
};

void f(void)
{
    my_pair<int, float> a(1, 2.3f);
    my_pair<int, float> b(a);
}

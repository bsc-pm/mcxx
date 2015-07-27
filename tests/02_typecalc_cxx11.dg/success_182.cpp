/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

struct my_pair
{
    typedef int first_type;
    typedef float second_type;

    int first;
    float second;

    constexpr my_pair()
        : first(), second() { }

    constexpr my_pair(int first, float second)
        : first(first), second(second) { }

    constexpr my_pair(const my_pair&) = default;
};

void f(void)
{
    constexpr my_pair a(1, 2.3f);
    constexpr my_pair b(a);

    static_assert(a.first == 1, "");
    static_assert(b.first == 1, "");
}

/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

template <typename ...T>
struct B;

typedef unsigned long size_t;

template <>
struct B<>
{
    static constexpr size_t value = 0;
};

template <typename T>
struct B<T>
{
    static constexpr size_t value = sizeof(T);
};

template <typename T, typename ...R>
struct B<T, R...>
{
    static constexpr size_t value = sizeof(T) + B<R...>::value;
};

void f()
{
    constexpr size_t v = B<int, double, float, long>::value;
    static_assert(v ==
             (sizeof(int) + sizeof(double) + sizeof(float) + sizeof(long)),
             "Something is amiss");
}

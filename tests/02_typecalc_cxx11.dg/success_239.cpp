/*
<testinfo>
test_generator="config/mercurium-cxx11"
test_nolink=yes
</testinfo>
*/

template <int ...V>
struct integer_sequence { };

template <typename ...T>
constexpr int sum(T ...args)
{
    return 2;
}

template <int ...Is>
constexpr int foo(integer_sequence<Is...>)
{
    return sum(Is...);
}

constexpr int a = foo(integer_sequence<0, 1>());

static_assert(a == 2, "");

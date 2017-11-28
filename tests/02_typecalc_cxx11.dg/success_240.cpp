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
    return sizeof...(args);
}

template <int ...Is>
constexpr int foo(integer_sequence<Is...>)
{
    return sum(Is...);
}

constexpr int a = foo(integer_sequence<3, 4>());

static_assert(a == 2, "");

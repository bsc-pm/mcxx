/*
<testinfo>
test_generator="config/mercurium-cxx11"
test_nolink=yes
</testinfo>
*/

template <int ...V>
struct integer_sequence { };

template <int ...Is>
constexpr int foo(integer_sequence<Is...>)
{
    return sizeof...(Is);
}

constexpr int a = foo(integer_sequence<0, 1, 2>());

static_assert(a == 3, "");


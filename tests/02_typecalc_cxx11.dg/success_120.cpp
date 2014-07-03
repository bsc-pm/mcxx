/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/
enum class E
{
    V1 = 1,
    V2 = 2,
    V3 = V1 | V2,
};

constexpr
E E_or(E e1, E e2)
{
    return static_cast<E>(
            static_cast<int>(e1)
            | static_cast<int>(e2));
}

constexpr
E operator|(E e1, E e2)
{
    return static_cast<E>(
            static_cast<int>(e1)
            | static_cast<int>(e2));
}

void g()
{
    static_assert(E_or(E::V1, E::V2) == E::V3, "Invalid value");
    static_assert((E::V1 | E::V2) == E::V3, "Invalid value");
}

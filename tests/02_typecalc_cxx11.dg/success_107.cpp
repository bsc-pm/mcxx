/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

template <typename Result>
constexpr Result sum_aux()
{
    return 0;
}

template <typename Result, typename Head, typename ...Tail>
constexpr Result sum_aux(Head head, Tail ...tail)
{
    return head + sum_aux<Result>(tail...);
}

template <typename Result, typename ...Numeric>
constexpr Result sum(Numeric... t)
{
    return sum_aux<Result>(t...);
}

void g()
{
    static_assert(sum<int>(1, 2, 3) == 6, "1 + 2 + 3 != 6");
}

/*
<testinfo>
test_generator="config/mercurium-cxx11"
test_nolink=yes
</testinfo>
*/

template <int S>
static constexpr int sum_aux()
{
    return S;
}

template <int S, int H, int ...N>
static constexpr int sum_aux()
{
    return sum_aux<S + H, N...>();
}


template <int ...N>
static constexpr int sum() { 
    return sum_aux<0, N...>();
}

static_assert(sum<1, 2, 3, 4>() == 10, "");

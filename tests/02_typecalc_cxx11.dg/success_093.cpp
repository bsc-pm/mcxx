/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

template <typename T>
struct Literal
{
    T t_;
    constexpr Literal(const T& t) : t_( 2 * t) { }
};

template <int N>
struct A
{
};

typedef A< Literal<int>(3).t_ > B;
typedef A< 6 > B;

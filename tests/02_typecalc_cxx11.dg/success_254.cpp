/*
<testinfo>
test_generator="config/mercurium-cxx11"
test_nolink=yes
</testinfo>
*/

template <typename T, typename Q>
struct SameType;

template <typename T>
struct SameType<T, T> { };

struct B
{
    int x;
    static int y;
};

void foo(const B& rb, const B *pb)
{
    SameType<int, decltype(rb.x)>();
    SameType<int, decltype(pb->x)>();

    SameType<const int&, decltype((rb.x))>();
    SameType<const int&, decltype((pb->x))>();

    SameType<int, decltype(rb.y)>();
    SameType<int, decltype(pb->y)>();

    SameType<int &, decltype((rb.y))>();
    SameType<int &, decltype((pb->y))>();
}

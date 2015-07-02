/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
template <typename T>
struct P
{
    operator T* () const;
};

template <typename T>
struct V
{
    const T& operator[](int x) const;
    T& operator[](int x);
};

struct A
{
    int x;
};

V<P<A> > c;

A* foo()
{
    return c[1];
}

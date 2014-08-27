/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T>
struct constant
{
    explicit constant(const T&);
};

template <typename T>
struct temporary : constant<T>
{
    explicit temporary(T& obj);
};

template <typename T>
struct enabled
{
    operator temporary<T>();
    operator constant<T>() const;
};

struct D : enabled<D>
{
    D(int);
};

void f(constant<D>);
void f(temporary<D>);

void g()
{
    f(D(3));
}

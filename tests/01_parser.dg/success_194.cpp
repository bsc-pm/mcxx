/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T, int n>
struct C
{
};

template <typename T>
struct B
{
    typedef T K;
};

struct D
{
    template <typename T, int n>
        operator C<typename B<T>::K, n>() const;
};

template <typename T>
struct A : D
{
    template <int n>
        operator C<typename B<T>::K, n>() const;
};

template <typename T>
template <int n>
A<T>::operator C<typename B<T>::K, n>() const
{
    return D::operator C<typename B<T>::K, n>();
}

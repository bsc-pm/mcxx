/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T>
struct A
{
    typedef void Type;
};

template <typename T>
typename A<T>::Type f(T t)
{
    return;
}

void g()
{
    f(3);
    f(3.4);
    f(3.4f);
}

/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T>
struct Foo;
template <typename T>
struct A
{
    static typename Foo<T>::Type1 f1();
    static typename Foo<T>::Type2 f2();
    static bool fb(void);

    __typeof__(fb() ? f1() : f2()) t;
};

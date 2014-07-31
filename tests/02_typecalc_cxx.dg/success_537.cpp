/*
<testinfo>
test_generator="config/mercurium"
</testinfo>
*/
template <typename T>
struct A
{
    static void f(const T& = T());
};

void g()
{
    A<signed int>::f();
    A<unsigned int>::f();

    A<char>::f();
    A<unsigned char>::f();
    A<signed char>::f();

    A<short>::f();
    A<unsigned short>::f();

    A<signed long>::f();
    A<unsigned long>::f();

    A<signed long long>::f();
    A<unsigned long long>::f();

    A<float>::f();
    A<double>::f();
    A<long double>::f();
}

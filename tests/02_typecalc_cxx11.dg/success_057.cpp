/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

struct my_true_type
{
    const static bool value = true;
};

struct my_false_type
{
    const static bool value = false;
};

template <typename T>
struct my_is_class
{
    template <typename S>
    static my_true_type f(int S::* p);

    template <typename S>
    static my_false_type f(...);

    typedef decltype(f<T>(0)) type;

    const static bool value = type::value;
};

template <int N>
struct is_zero { };

template <>
struct is_zero<0> { typedef int T; };

template <int N>
struct is_one { };

template <>
struct is_one<1> { typedef int T; };


struct B { };

void f(void)
{
    is_zero<my_is_class<B*>::value >::T *t1;
    is_one<my_is_class<B>::value >::T *t2;

    t1 == t2;
}

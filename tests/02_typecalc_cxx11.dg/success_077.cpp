/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

struct false_type { };
struct true_type { };

template <typename T, typename S>
struct CheckSameType
{
    typedef false_type Type;
};

template <typename T>
struct CheckSameType<T, T>
{
    typedef true_type Type;
};

template <typename T>
struct CheckHolds;

template <>
struct CheckHolds<true_type>
{
    typedef true_type Type;
};

template <typename T>
struct CheckDoesNotHold;

template <>
struct CheckDoesNotHold<false_type>
{
    typedef true_type Type;
};

template <typename T>
struct A
{
    template <typename Q>
        using Test = typename CheckSameType<Q, T>::Type;

    // Constructor for the same type
    template <typename M, typename _Test = typename CheckHolds<Test<M>>::Type >
        void foo(const M&);

    // Constructor for another type
    template <typename M, typename _Test = typename CheckDoesNotHold<Test<M>>::Type >
        void foo(const M&, const M&);
};

void f()
{
    A<int> a;
    a.foo(2);

    a.foo(3.4f, 5.6f);
}

/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <typename T>
constexpr bool f(T*) { return true; }

template <typename T>
constexpr bool f(...) { return false; }

constexpr bool b = f<int>(0);

template <bool B>
struct CheckTrue;

template <>
struct CheckTrue<true>
{
    typedef int T;
};

template <bool B>
struct CheckFalse;

template <>
struct CheckFalse<false>
{
    typedef int T;
};

constexpr bool b2 = f<int>(1, 2, 3);

CheckTrue<b>::T t1;
CheckFalse<b2>::T t2;

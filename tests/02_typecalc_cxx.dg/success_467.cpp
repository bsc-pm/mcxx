/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template< bool C_ > struct bool_;
typedef bool_<true> true_;
typedef bool_<false> false_;

template< bool C_ > struct bool_
{
    static const bool value = C_;
    typedef bool_ type;
};


template <typename T>
struct A
{
    typedef typename T::type P;
    enum { Value = P::value };
};

void g()
{
    A< true_ > :: Value;
    A< false_ > :: Value;
}

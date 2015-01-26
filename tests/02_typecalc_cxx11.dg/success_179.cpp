/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/
template <typename T, typename S>
struct A
{
};

struct  __sfinae_types
{
    typedef char __one;
    struct  __two
    {
        char __arr[2];
    };
};


template < typename _Tp >
struct  _Derives_from_A : __sfinae_types
{
    private:
        template < typename _T, typename _S >
            static __sfinae_types::__one __test(const volatile A<_T, _S> *);
        static __sfinae_types::__two __test(...);
    public:
        // This test verifies that the call to __test does not become
        // (*this).__test which is wrong
        static const bool value = sizeof(__test((_Tp *)0)) == 1;
};

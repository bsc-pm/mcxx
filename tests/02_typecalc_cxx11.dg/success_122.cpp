/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

template<typename _Res>
struct future;

template<typename _Res>
struct __basic_future
{
    __basic_future();
    __basic_future(const __basic_future&) = delete;
    __basic_future(future<_Res>&&) noexcept;
};

template <typename T>
struct remove_reference {
    typedef T type;
};

template <typename T>
struct remove_reference<T&> {
    typedef T type;
};

template <typename T>
struct remove_reference<T&&> {
    typedef T type;
};


template<typename _Tp>
constexpr typename remove_reference<_Tp>::type&&
move(_Tp&& __t) noexcept
{ return static_cast<typename remove_reference<_Tp>::type&&>(__t); }


template<>
struct future<void> : public __basic_future<void>
{
    typedef __basic_future<void> _Base_type;
    public:
    future(future&& __uf) noexcept : _Base_type(move(__uf)) { }
    future() : _Base_type() { }
};

void g()
{
    future<void> f;
    future<void> f2(move(f));
}

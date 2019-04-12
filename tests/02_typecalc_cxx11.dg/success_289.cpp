/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/
template<typename _Alloc>
struct allocator_traits
{
    template<typename...> using __void_t = void;

    template<template<typename> class _Func, typename = void >
     struct _Ptr
     {
     };
    template<template<typename> class _Func>
    struct _Ptr<_Func, __void_t< _Func<_Alloc> >>
    {
    };
};

int main()
{
    allocator_traits<int> a;
}

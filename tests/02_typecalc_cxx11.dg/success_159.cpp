/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

template<typename _Alloc, typename _Tp>
struct __alloctr_rebind
{
    typedef typename _Alloc::template rebind2<_Tp>::other2 __type;
};

template<typename _Alloc>
struct allocator_traits
{
    template<typename _Tp>
        using rebind_alloc = typename __alloctr_rebind<_Alloc, _Tp>::__type;
};

template<typename _Alloc>
  struct __alloc_traits

  : allocator_traits<_Alloc>
{
    typedef allocator_traits<_Alloc> _Base_type;

    template<typename _Tp>
        struct rebind
        { typedef typename _Base_type::template rebind_alloc<_Tp> other1; };
};

template <typename _Tp>
struct allocator
{
    template <typename _Sp>
        struct rebind2 {
            typedef allocator<_Sp> other2;
        };
};

__alloc_traits<allocator<int>>::rebind<int>::other1 o;

typedef int ptrdiff_t;
typedef unsigned int size_t;
namespace A
{
  using ::ptrdiff_t;
  using ::size_t;
}


namespace A
{
  template<typename _Alloc>
    struct allocator;
  template<class _CharT>
    struct char_traits;
  template<typename _CharT, typename _Traits = char_traits<_CharT>,
           typename _Alloc = allocator<_CharT> >
    struct basic_string;
  template<> struct char_traits<char>;
  typedef basic_string<char> string;
  template<> struct char_traits<wchar_t>;
  typedef basic_string<wchar_t> wstring;
}

namespace A
{
  template<typename _CharT, typename _Traits, typename _Alloc>
    struct basic_string
    {
        typedef typename _Alloc::template rebind<_CharT>::other _CharT_alloc_type;
        typedef typename _CharT_alloc_type::size_type size_type;
    };
}

namespace B
{
  template<typename _Tp>
    class new_allocator
    {
    };
}

namespace A
{
  template<typename _Tp>
    struct allocator;
  template<>
    struct allocator<void>
    {
    public:
      typedef size_t size_type;
      template<typename _Tp1>
        struct rebind
        { typedef allocator<_Tp1> other; };
    };
  template<typename _Tp>
    struct allocator: public B::new_allocator<_Tp>
    {
   public:
      typedef size_t size_type;
      template<typename _Tp1>
        struct rebind
        { typedef allocator<_Tp1> other; };
      allocator(const allocator& __a) throw()
      : B::new_allocator<_Tp>(__a) { }
    };
}

namespace A
{
    void g()
    {
         typedef string::size_type size_type;
    }
}

/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <class T> struct type_identity { using type = T; };

// Not the real thing
template <class T> struct add_rvalue_reference : type_identity<T &&> {};

template<class T>
typename add_rvalue_reference<T>::type declval() noexcept;

struct true_type { static constexpr bool value = true; };
struct false_type { static constexpr bool value = false; };

struct __do_is_destructible_impl {
  template <typename _Tp, typename = decltype(declval<_Tp>().~_Tp())>
  static true_type __test(int);

  template <typename> static false_type __test(...);
};

template <typename _Tp>
struct __is_destructible_impl : public __do_is_destructible_impl {
  typedef decltype(__test<_Tp>(0)) type;
};

struct A { };

static_assert(__is_destructible_impl<A>::type::value, "");
static_assert(!__is_destructible_impl<A&>::type::value, "");
static_assert(__is_destructible_impl<const A>::type::value, "");
static_assert(!__is_destructible_impl<const A&>::type::value, "");

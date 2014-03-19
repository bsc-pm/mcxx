/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

namespace boost
{
    template <typename T>
    struct remove_reference;

    template <typename T>
    struct remove_cv;

    template <typename T>
    struct remove_pointer;

    template < typename Lhs, typename Rhs, typename Ret, bool Forbidden_if >
        struct trait_impl1;

    namespace type_traits
    {
        template <bool b1, bool b2, bool b3 = true, bool b4 = true, bool b5 = true, bool b6 = true, bool b7 = true>
            struct ice_and;

        template <bool b1, bool b2, bool b3 = true, bool b4 = true, bool b5 = true, bool b6 = true, bool b7 = true>
            struct ice_or;

        template <bool b>
            struct ice_not ;
    }

    template <typename T>
        struct is_fundamental;

    template <typename T>
        struct is_integral;

    template <typename T>
        struct is_pointer;
    

template < typename Lhs, typename Rhs, typename Ret >
struct trait_impl {
   typedef typename ::boost::remove_reference<Lhs>::type Lhs_noref;
   typedef typename ::boost::remove_reference<Rhs>::type Rhs_noref;
   typedef typename ::boost::remove_cv<Lhs_noref>::type Lhs_nocv;
   typedef typename ::boost::remove_cv<Rhs_noref>::type Rhs_nocv;
   typedef typename ::boost::remove_cv< typename ::boost::remove_reference< typename ::boost::remove_pointer<Lhs_noref>::type >::type >::type Lhs_noptr;
   typedef typename ::boost::remove_cv< typename ::boost::remove_reference< typename ::boost::remove_pointer<Rhs_noref>::type >::type >::type Rhs_noptr;
   static const bool value = (trait_impl1 < Lhs_noref,
           Rhs_noref,
           Ret,
           ::boost::type_traits::ice_or< ::boost::type_traits::ice_and< ::boost::is_fundamental< Lhs_nocv >::value,
           ::boost::is_fundamental< Rhs_nocv >::value,
           ::boost::type_traits::ice_or< ::boost::type_traits::ice_not< ::boost::is_integral< Lhs_noref >::value >::value,
           ::boost::type_traits::ice_not< ::boost::is_integral< Rhs_noref >::value >::value >::value >::value,
           ::boost::type_traits::ice_and< ::boost::is_fundamental< Lhs_nocv >::value,
                                          ::boost::is_pointer< Rhs_noref >::value >::value,
           ::boost::type_traits::ice_and< ::boost::is_fundamental< Rhs_nocv >::value,
                                           ::boost::is_pointer< Lhs_noref >::value >::value,
           ::boost::type_traits::ice_and< ::boost::is_pointer< Lhs_noref >::value,
           ::boost::is_pointer< Rhs_noref >::value >::value >::value >::value);
};

}

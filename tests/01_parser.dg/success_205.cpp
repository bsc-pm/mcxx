/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

struct A {};

template <typename BaseT>
struct GenericFactory {
   template<class T, class S>
   bool register_subtype();
};

template <typename T, typename Q>
void foo( GenericFactory<A> & mask_factory )
{
   mask_factory.register_subtype<T, Q> ();
}

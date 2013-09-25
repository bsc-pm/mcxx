/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

struct Factory
{
   template <typename U>
   bool register_subtype(int num);
};

struct TopologyModule
{
   Factory mask_factory;

   template<class T>
   bool register_mask()
   {
      return mask_factory.register_subtype<T>(T::num);
   }
};

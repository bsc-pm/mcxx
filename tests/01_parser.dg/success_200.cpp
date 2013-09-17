/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T>
struct A { };

template <typename T>
class B
{
   A<T>::_t;

   public:
   B() { _t = 0; }
};

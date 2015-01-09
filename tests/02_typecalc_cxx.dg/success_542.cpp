/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
template < typename T>
struct A
{
    typedef void (A::*bool_type)();
    operator bool_type() const
    {
        return 0;
    }
};
template < typename T>
struct  B : public A<T>
{
    typedef A<T> base;

    typedef typename A<T>::bool_type bool_type;
    operator bool_type() const
    {
      return base::operator bool_type();
    }
};

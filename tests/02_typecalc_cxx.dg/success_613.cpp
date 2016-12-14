/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
class Base
{
    enum E {};
};

template<class C>
class MyClass : public Base
{
    typedef enum C::E MyE;
};

/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
struct A {
   void foo(void);
};

template <typename T>
struct B : public T
{
   using A::foo;
};

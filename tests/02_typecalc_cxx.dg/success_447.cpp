/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

struct Resource
{
    void unref();
    static void unref(int);
};

struct A
{
   void foo() {
      Resource::unref(1);
   }
};

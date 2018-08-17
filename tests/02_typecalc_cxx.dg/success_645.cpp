/*
<testinfo>
test_generator="config/mercurium"
test_nolink=yes
</testinfo>
*/

struct MyData
{
    template <typename T2>
   void allocate()
   {}
};

template <typename T>
struct Derived
{
    MyData _result;
    void bar() {
        _result.allocate<T>(); // For this code what Mercurium generates is
                               // (*this)._result.template allocate<T>();
        (*this)._result.template allocate<T>();
        _result.template allocate<T>();
    }
};


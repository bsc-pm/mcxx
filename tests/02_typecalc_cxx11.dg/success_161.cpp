/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

template <typename T>
struct B
{
    struct C
    {
        static T foo();
        typedef decltype(foo()) type;
    };
};

void f()
{
    B<int>::C::type b;
}

/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
namespace A
{
    template <typename _T>
        struct B { 
            _T t;
        };

}

template class A::B<int>;

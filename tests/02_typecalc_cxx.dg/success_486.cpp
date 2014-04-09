/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
namespace my_std
{
    template <typename T>
        struct value_type_trait
        {
            typedef T type;
        };
    template <typename T>
        struct my_vector
        {
            typedef typename value_type_trait<T>::type value_type;
        };
}

template <int N>
class SpatialContainersConfigure
{
    typedef int Foo;
    typedef my_std::my_vector<Foo> ContainerContactType;
    typedef ContainerContactType::value_type PointerContactType;
};

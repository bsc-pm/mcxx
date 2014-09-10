/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
template <typename T>
struct A
{
};

template <>
struct A<int>
{
    A();
    ~A<int>();
};

template <>
struct A<float>
{
    A() { }
    ~A<float>() { }
};

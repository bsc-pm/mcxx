/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <typename T>
struct Foo;

template <typename T>
struct alignas(T**) alignas(T*) Foo
{
};

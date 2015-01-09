/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <typename ...T>
struct A
{
};

template <typename ...>
struct B
{
};

template <int ...N>
struct C
{
};

template <int ...>
struct D
{
};

template <template <typename> class ...W>
struct E
{
};

template <template <typename> class ...>
struct F
{
};

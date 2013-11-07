/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <typename T, typename S>
struct W1
{
};

template <typename T, typename S>
struct W2
{
};

template <template <typename T, typename S> class ...W>
struct A
{
};

A<W1, W2> a;

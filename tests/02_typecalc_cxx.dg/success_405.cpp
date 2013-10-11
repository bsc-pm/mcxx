/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T>
struct A
{
};

void f(A<int>* a)
{
    delete a;
}

void f(A<float>* a)
{
    delete[] a;
}

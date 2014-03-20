/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
template <typename T>
struct A
{
};

template <typename T>
struct B : A<T>
{
};

void g()
{
    B<int>* p;
    A<int>* q = p;

    q = p;
}

/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
template <typename T>
struct A
{
    A();
    ~A();
};

void f(A<int> *a)
{
    a->A<int>::~A<int>();
    a->A<int>::~A();
}

template <typename T>
void g(A<T> *a)
{
    (*a).A<T>::~A();
    (*a).A<T>::~A<T>();
}

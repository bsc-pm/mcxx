template <typename _T>
struct A
{
    struct B
    {
        B* f();
    };
};

template <typename _T>
typename A<_T>::B* A<_T>::B::f()
{
}

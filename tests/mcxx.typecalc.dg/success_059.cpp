template <typename _T>
struct A
{
    struct B
    {
        void f();
    };
};

template <typename _T>
void A<_T>::B::f()
{
    B* p;

    p->k = 3;
}

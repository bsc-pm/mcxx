template <typename _T>
struct A
{
};


template <template <typename> class _V,
         typename _T>
_V<_T*>* pointer_to_template(_V<_T>*);

void f()
{
    A<int> *k1 = 0;
    A<int*> *k2;

    k2 = pointer_to_template(k1);
}

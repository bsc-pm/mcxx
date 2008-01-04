struct A
{
    template <typename _T, typename _Q>
        void f(_T t, _Q q);

    template <typename _T, typename _Q>
        void f(_T* t, _Q* q);
};

template <>
void A::f<int>(int, float) { }

template <>
void A::f<int*>(int*, float*) { }

template <>
void A::f(int*, float*) { }

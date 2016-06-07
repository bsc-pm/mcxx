/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
template<typename T>
struct traits;

template<typename T>
struct POB
{
    enum { ENUMERATOR_A1 = traits<T>::ENUMERATOR_C1 > 0 };
};

template<int n>
struct DS : public POB< DS<n> >
{
};

template<int n>
struct traits< DS<n> >
{
    enum {
        ENUMERATOR_B1  = 0
    };
    enum {
        ENUMERATOR_C1 = ::traits < DS < n > > ::ENUMERATOR_B1 | 0,
    };
};

void foo() {
    traits<DS<0> > a;
}

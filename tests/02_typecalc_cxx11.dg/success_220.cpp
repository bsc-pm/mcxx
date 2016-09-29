/*
<testinfo>
test_generator="config/mercurium-cxx11"
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
    // Since we shouldn't need to instantiate this class, we can emit an error if we instantiate it
    static_assert(n != 0, "Error!");
};

template<int n>
struct traits<DS<n> >
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

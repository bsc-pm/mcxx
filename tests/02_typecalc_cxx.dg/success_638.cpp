/*
<testinfo>
test_generator="config/mercurium"
</testinfo>
*/
template <typename T>
struct A {
    typedef T type;
};

template <typename T>
struct B : A<T> {

    using typename A<T>::type;

    typename type::reference foo()
    {
    }
};


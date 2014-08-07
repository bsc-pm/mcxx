/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

template <typename T>
struct my_remove_reference { typedef T type; };

template <typename T>
struct my_remove_reference<T&> { typedef T type; };

template <typename T>
struct my_remove_reference<const T&> { typedef const T type; };

template <typename T>
void my_forward(typename my_remove_reference<T>::type&);

template <typename T>
void my_forward(typename my_remove_reference<T>::type&&);

struct A
{
};

void g()
{
    const A ka;
    my_forward<const A&>(ka);
}

/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename Target>
struct A
{
    template <template <typename T> class Derived>
        Derived<Target>* as();
};

template <typename Target>
template <template <typename T> class Derived>
Derived<Target>* A<Target>::as()
{
}

/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <bool>
struct A;

template <>
struct A<true> { int x; };

template <typename T>
void g()
{
    sizeof(A<( ((false && sizeof(T)) == 0) ? false : true)>);
    sizeof(A<( ((true || sizeof(T)) == 0) ? false : true)>);
}

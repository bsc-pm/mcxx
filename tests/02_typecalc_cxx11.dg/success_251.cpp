/*
<testinfo>
test_generator="config/mercurium-cxx11"
test_nolink=yes
</testinfo>
*/

template < typename T>
struct is_X
{
    static constexpr bool value = false;
};

template<bool b, typename T, typename... Tail>
struct A
{
    typedef int type;
};

    template<typename T, typename... Tail>
inline typename A< is_X<T>::value,  T,  Tail...>::type bar(T&& __f, Tail&&... args)
{ }

int main() {
    int x;
    bar(x);
}

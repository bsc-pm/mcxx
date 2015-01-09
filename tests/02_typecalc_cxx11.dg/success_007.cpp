/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <typename ...T>
void f(T ...t);

template <typename ...T>
void g(T ...t)
{
    f(t...);
}

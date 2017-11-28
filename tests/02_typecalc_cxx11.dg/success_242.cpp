/*
<testinfo>
test_generator="config/mercurium-cxx11"
test_nolink=yes
</testinfo>
*/

template <int ...N>
void foo()
{
}

template <typename ...T>
void bar()
{
    foo<sizeof(T)...>();
}

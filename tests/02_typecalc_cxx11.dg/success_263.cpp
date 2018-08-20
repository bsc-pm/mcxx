/*
<testinfo>
test_generator="config/mercurium-cxx11"
test_nolink=yes
</testinfo>
*/
template < typename ...Args>
void foo(Args... args)
{
    [&]() { bar(args...); };
}

template < typename ...Args>
void bar(Args... args)
{
}

/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/
class A
{
    typedef int MyInt;
};

template<typename T>
void foo(T a)
{
    sizeof(decltype(a)::MyInt);
}

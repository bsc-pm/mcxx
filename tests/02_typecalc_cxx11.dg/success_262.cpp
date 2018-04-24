/*
<testinfo>
test_generator="config/mercurium-cxx11"
test_nolink=yes
</testinfo>
*/
#include<initializer_list>

template < typename T>
struct MyVector
{
    MyVector(const std::initializer_list<T>&);
};

template < typename T1, typename T2>
T1 foo(const T2 &var)
{
    return T1(var);
}

int main()
{
    foo< MyVector<unsigned long int> >(::std::initializer_list<unsigned long int>({ 1, 2 }));
}

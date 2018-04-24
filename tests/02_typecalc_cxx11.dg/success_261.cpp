/*
<testinfo>
test_generator="config/mercurium-cxx11"
test_nolink=yes
</testinfo>
*/
#include<initializer_list>

int main()
{
    std::initializer_list<int> v1({1,2,3});
    std::initializer_list<int> v2{1,2,3};
    std::initializer_list<int> v3(v2);
    std::initializer_list<int> v4{v2};
}

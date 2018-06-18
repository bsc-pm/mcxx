/*
<testinfo>
test_generator="config/mercurium-cxx11"
test_nolink=yes
</testinfo>
*/

#if defined(__GNUC__)
#if __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 9)
#include<initializer_list>

int main()
{
    std::initializer_list<int> v1({1,2,3});
    std::initializer_list<int> v2{1,2,3};
    std::initializer_list<int> v3(v2);
    std::initializer_list<int> v4{v2};
}

#endif
#endif

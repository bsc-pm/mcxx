/*
<testinfo>
test_generator="config/mercurium-cxx11"
test_nolink=yes
</testinfo>
*/
#include<initializer_list>

int main() {
    std::initializer_list<int> a1 = {1};
    std::initializer_list<int> a2{1};
}

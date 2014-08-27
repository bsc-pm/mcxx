/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

#include <initializer_list>

struct B { B(std::initializer_list<int>); };

struct A { B p; } a = { { 1 , 2, 3 } };

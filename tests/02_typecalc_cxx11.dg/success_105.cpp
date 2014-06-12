/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

#include <initializer_list>

struct B { B(std::initializer_list<int>); };
struct C { C(double, int, int); };
struct D { int x, y, z; };

struct E { E(B); };

B b{1,2,3};
C c{1.3,2,3};
D d{1,2,3};

E e{{1,2,3}};

/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

struct C;

typedef int D;
struct A;

C * (*foo())(D) { return 0; }
C * (A::* bar()) ( D ) { return 0; }
C * (*quux())[10] { return 0; }
C * (A::* meanie())[20] { return 0; }

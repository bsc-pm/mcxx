/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

struct T1 { };
struct T2 { };
struct T3 { };
struct T4 { };

T3 operator>>(T1, T2);
void operator>>(T4, T3);

void f()
{
    T1 t1;
    T2 t2;
    T4 t4;

    t4 >> (t1 >> t2);
}

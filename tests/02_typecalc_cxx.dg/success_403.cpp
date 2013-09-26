/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

enum E1 { E1_1, E1_2 };
enum E2 { E2_1, E2_2 };

E1 operator|(E1, E1);
E2 operator|(E2, E2);
E1 operator~(E1);
E2 operator~(E2);

void f()
{
    ~(E1_1 | E1_2);
}

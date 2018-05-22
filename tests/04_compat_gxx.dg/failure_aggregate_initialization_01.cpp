/*
<testinfo>
test_generator=config/mercurium
test_compile_fail=yes
</testinfo>
*/

typedef struct
{
    int x;
} A;


typedef struct {
    A a;
    int y;
} B;

static B b = {
    .a = { .x = 0 },
    .y = 2,
    .c = 0
};

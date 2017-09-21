/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
typedef int (*p_array_t)[2];

int (*f1())[2];
p_array_t f1() {}

int (*f2())[2] {}

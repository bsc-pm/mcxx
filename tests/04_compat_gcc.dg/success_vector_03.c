/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

typedef __attribute__((vector_size(16))) float float4;

void f(void)
{
    float x;
    float4 v;

    v[2] = 3;
    x = v[2];
}

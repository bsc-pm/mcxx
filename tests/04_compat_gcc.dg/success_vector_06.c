/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

static const  float _ps_1[4] __attribute__((aligned(16))) = { 1.0f, 1.0f, 1.0f, 1.0f };
typedef float __attribute__((vector_size(16))) v4sf;

void f()
{
    v4sf a = *(v4sf*)_ps_1;
}

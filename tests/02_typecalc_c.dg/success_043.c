/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
_Complex int i_a = 3i;
_Complex int i_c = 3j;

_Complex unsigned int ui_a = 3iu;
_Complex unsigned int ui_c = 3ju;

_Complex long l_a = 3il;
_Complex long l_c = 3jl;

_Complex double d_a = 3.0i;
_Complex double d_c = 3.0j;

_Complex float f_a = 3.0fi;
_Complex float f_c = 3.0fj;

void f(void)
{
    i_a + i_c;
    i_a = i_c;
    i_a += i_c;

    f_a + f_c;
    f_a = f_c;
    f_a += f_c;

    f_a = d_a;
    d_a = f_a;

    f_a = i_a;
}

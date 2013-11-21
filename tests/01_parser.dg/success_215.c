/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

typedef enum
{ HPL_INT = 100,
  HPL_DOUBLE = 101
}
HPL_T_TYPE;

int HPL_all_reduce(const HPL_T_TYPE);

int HPL_all_reduce(const HPL_T_TYPE a)
{
    return 0;
}

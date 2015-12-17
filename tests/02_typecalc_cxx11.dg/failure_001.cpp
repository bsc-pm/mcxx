/*
<testinfo>
test_generator=config/mercurium-fe-only
test_compile_fail=yes
</testinfo>
*/

struct C
{
    int k;
};

C c( { 1, { 4 } } );


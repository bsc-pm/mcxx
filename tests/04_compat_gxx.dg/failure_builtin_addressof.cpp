/*
<testinfo>
test_generator=config/mercurium-fe-only
test_compile_fail=yes
</testinfo>
*/
void foo() {
    int *p = __builtin_addressof((int)2);

}

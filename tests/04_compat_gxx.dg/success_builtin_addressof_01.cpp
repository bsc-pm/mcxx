/*
<testinfo>
test_generator=config/mercurium-fe-only
</testinfo>
*/
void foo() {
    int x;
    int *p = __builtin_addressof(x);
}

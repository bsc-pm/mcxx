/*
<testinfo>
test_generator=config/mercurium-fe-only
test_compile_fail=yes
</testinfo>
*/
int main(int argc, char argv[])
{
    char* t0;
    const char* t1;
    t0 = t1; // ERROR!
}

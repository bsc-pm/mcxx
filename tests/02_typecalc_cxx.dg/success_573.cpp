/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
void bar(const char*);

void foo()
{
    const char* c = __PRETTY_FUNCTION__;
    bar(c);
}

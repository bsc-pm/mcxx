/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

void f(void)
{
    char16_t a;
    char32_t b;

    a + a;
    b + b;

    a + b;

    int i;

    a + i;
    b + i;

    short s;

    a + s;
    b + s;
}

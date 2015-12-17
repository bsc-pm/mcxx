/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
void g1(const char*, int, const char*);

void g2(char, int, char);

void f(int abc)
{
    g1("\"", abc, "\"");
    g2('\'', abc, '\'');
}

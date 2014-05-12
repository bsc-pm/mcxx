/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

void f(int);
void f(float);

struct Type
{
    Type(void (*p)(int));
};

void h()
{
    Type * t = new Type(f);
}

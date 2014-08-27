/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

struct constant
{
    explicit constant(const int&);
};

struct temporary : constant
{
    explicit temporary(int& obj);
};

struct enabled
{
    operator temporary();
    operator constant() const;
};

struct D : enabled
{
    D(int*);
};

void f(constant);
void f(temporary);

D h();

void g()
{
    (( f(h()) ));
}

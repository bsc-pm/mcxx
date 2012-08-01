/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
typedef struct ATOM_t { int x; } ATOM;
void f(x)
    int x;
{
    ATOM *new, *a_m_serial();

    new = a_m_serial(x);
}

ATOM* a_m_serial(x)
    int x;
{
    return 0;
}

void f2(x)
    int x;
{
    ATOM *new, *a_m_serial();

    new = a_m_serial(x);
}


struct C_tag
{
    float c;
};

typedef struct C_tag C;

typedef C *p_C;

typedef p_C p2_C;

void f(void)
{
    float f = 0.0f;
    double d = 0.0;

    struct C_tag *c1;

    c1->c = 3;
    c1->c = 3.4f;
    c1->c = 3.4;
    c1->c = f;
    c1->c = d;

    C *c2;

    c2->c = 3;
    c2->c = 3.4f;
    c2->c = 3.4;
    c2->c = f;
    c2->c = d;

    p_C c3;

    c3->c = 3;
    c3->c = 3.4f;
    c3->c = 3.4;
    c3->c = f;
    c3->c = d;

    p2_C c4;

    c4->c = 3;
    c4->c = 3.4f;
    c4->c = 3.4;
    c4->c = f;
    c4->c = d;
}

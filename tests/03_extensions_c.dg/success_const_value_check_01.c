/*
<testinfo>
test_generator=config/mercurium-extensions
</testinfo>
*/
const int t00 = 1;
const float t01 = 1.2f;

const int t1[3] = { 1, 2 };
const int t2[3] = { [1] = 2, [0] = 1 };

struct A { int x; int y; };

const struct A t3 = { 1, 2 };

const struct A t4 = { .y = 2, .x = 1 };

const struct A t5[3] = { { 1, 2 }, { 3, 4 } };

const struct A t6[3] = { [1] = { 3, 4 }, [0] = { 1, 2 } };

const struct A t7[3] = { [1] = { 3, 4 }, [0] = { 1, 2 } };

union U { double d; float f; };

const union U t8 = { 1 };

const int t9[] = {1, 2, 3};

const int t10[] = {[1] = 1, [5] = 5};

struct B { int x[2]; };

const struct B t11[2] = {{1, 2}, {3, 4}};

struct C { int x; };

struct D { struct C c; };

const struct D t12 = { {1} };

void h(void)
{
    @const-value-check@(t00);
    @const-value-check@(t01);

    @const-value-check@(t1);
    @const-value-check@(t2);

    @const-value-check@(t1[0]);
    @const-value-check@(t1[1]);

    @const-value-check@(t2[0]);
    @const-value-check@(t2[1]);

    @const-value-check@(t3);
    @const-value-check@(t3.x);
    @const-value-check@(t3.y);

    @const-value-check@(t5);
    @const-value-check@(t5[0]);
    @const-value-check@(t5[1]);
    @const-value-check@(t5[0].x);
    @const-value-check@(t5[1].x);
    @const-value-check@(t5[0].y);
    @const-value-check@(t5[1].y);

    @const-value-check@(t8); // this one includes unknown members
    @const-value-check@(t8.d);

    @const-value-check@(t9[0]);
    @const-value-check@(t9[1]);
    @const-value-check@(t9[0]);

    @const-value-check@(t10[0]);
    @const-value-check@(t10[1]);
    @const-value-check@(t10[2]);
    @const-value-check@(t10[3]);
    @const-value-check@(t10[4]);
    @const-value-check@(t10[5]);

    @const-value-check@(t11);
    @const-value-check@(t11[0]);
    @const-value-check@(t11[1]);
    @const-value-check@(t11[0].x);
    @const-value-check@(t11[1].x);
    @const-value-check@(t11[0].x[0]);
    @const-value-check@(t11[0].x[1]);
    @const-value-check@(t11[1].x[0]);
    @const-value-check@(t11[1].x[1]);

    @const-value-check@(t12);
    @const-value-check@(t12.c);
    @const-value-check@(t12.c.x);
}

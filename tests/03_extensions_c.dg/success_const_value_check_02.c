/*
<testinfo>
test_generator=config/mercurium-extensions
</testinfo>
*/
typedef
struct A
{
    int x, y;
} A;

void g()
{
    const A k = { 1, 2 };

    const A a[] = {1, 2};
    const A c[] = { k, k };

    @const-value-check@(a);
    @const-value-check@(a[0]);
    @const-value-check@(a[0].x);
    @const-value-check@(a[0].y);

    @const-value-check@(c);
    @const-value-check@(c[0]);
    @const-value-check@(c[0].x);
    @const-value-check@(c[0].y);
    @const-value-check@(c[1]);
    @const-value-check@(c[1].x);
    @const-value-check@(c[1].y);
}


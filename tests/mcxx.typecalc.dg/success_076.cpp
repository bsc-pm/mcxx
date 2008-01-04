void f()
{
    {
        int *p, *q;

        p == q;
        p < q;
        p <= q;
        p > q;
        p >= q;

        p - q;

        p == 0;
        p != 0;
    }
    {
        int p[10], *q;

        p == q;
        p < q;
        p <= q;
        p > q;
        p >= q;

        p - q;

        p == 0;
        p != 0;
    }

    {
        int *p, q[10];

        p == q;
        p < q;
        p <= q;
        p > q;
        p >= q;

        p - q;
    }
    {
        int p[10], q[10];

        p == q;
        p < q;
        p <= q;
        p > q;
        p >= q;

        p - q;
    }
}

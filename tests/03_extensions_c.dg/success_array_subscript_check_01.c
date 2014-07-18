/*
<testinfo>
test_generator=config/mercurium-extensions
</testinfo>
*/
int foo()
{
    {
        unsigned int m[100][200];

        @array-subscript-check@(m[1][2], 2);
        @array-subscript-check@(m[1][2:2], 2);
        @array-subscript-check@(m[1:1][2], 2);
        @array-subscript-check@(m[1:1][2:2], 2);

        @array-subscript-check@(m[1][2;2], 2);
        @array-subscript-check@(m[1;1][2], 2);
        @array-subscript-check@(m[1;1][2;2], 2);
    }

    {
        unsigned int (*m)[100][200];

        @array-subscript-check@((*m)[1][2], 2);
        @array-subscript-check@((*m)[1][2:2], 2);
        @array-subscript-check@((*m)[1:1][2], 2);
        @array-subscript-check@((*m)[1:1][2:2], 2);

        @array-subscript-check@((*m)[1][2;2], 2);
        @array-subscript-check@((*m)[1;1][2], 2);
        @array-subscript-check@((*m)[1;1][2;2], 2);
    }

    {
        unsigned int **m;

        @array-subscript-check@(m[1][2], 1);
        @array-subscript-check@(m[2][1:1], 1);
        @array-subscript-check@(m[1:2][2], 1);
        @array-subscript-check@(m[2][1;1], 1);
        @array-subscript-check@(m[1;1][2], 1);
    }

    {
        unsigned int *m[10];

        @array-subscript-check@(m[1][2], 1);
        @array-subscript-check@(m[2][1:1], 1);
        @array-subscript-check@(m[1:2][2], 1);
        @array-subscript-check@(m[2][1;1], 1);
        @array-subscript-check@(m[1;2][2], 1);
    }
}

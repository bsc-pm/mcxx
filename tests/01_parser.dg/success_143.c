/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

struct A
{
    int X;
};

# define YYRELOC(YYFROMITEMS,YYTOITEMS,YYX,YYTYPE) \
  &((YYTOITEMS) - ((YYFROMITEMS) - (struct A*) (YYX)))->YYTYPE

void f(struct A* a, struct A* b, void *c)
{
    YYRELOC(a, b, c, X);
}

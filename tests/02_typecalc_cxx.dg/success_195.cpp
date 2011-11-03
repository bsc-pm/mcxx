struct foo
{
    int k;
};
void foo(void)
{
}
void moo(struct foo* f)
{
    f->k = 3;
}

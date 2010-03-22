/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
template <typename _T>
void foo(_T);

template <typename _Fun>
void g(_Fun);

struct Bar
{
};

void moo()
{
    g(foo<Bar>);
}

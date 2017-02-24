/*
 <testinfo>
 test_generator=config/mercurium
 </testinfo>
 */
struct A
{
        static int foo, bar;
};

int cle;


template<int *p>
void f1()
{ }

int main()
{
        f1<&A::foo>();
            f1<&A::bar>();
                f1<&cle>();
}

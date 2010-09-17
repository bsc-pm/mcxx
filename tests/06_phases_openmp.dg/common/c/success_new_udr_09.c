/*
<testinfo>
test_generator=config/mercurium-omp
</testinfo>
*/


enum A
{
  L=1,
  D=2
};
#pragma omp declare reduction (foo: enum A: _out=_in)

int main (int argc, char* argv[])
{
	enum A a;
	#pragma omp parallel reduction(foo: a)
    {}

    return 0;
}


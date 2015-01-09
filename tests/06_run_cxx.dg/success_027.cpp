/*
<testinfo>
test_generator=config/mercurium-run
</testinfo>
*/

typedef unsigned long size_t;

int main(int argc, char* argv[])
{
	size_t i = 1;
	size_t step = 2;
	size_t newSize = 3;

    i < step ? i += newSize - step  : i -= step;

    return !( i == 2 );
}

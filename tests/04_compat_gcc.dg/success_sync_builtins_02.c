/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

int main(int argc, int *argv[])
{
	int i = 0x01;
	const int mask = 0x02;
    int before = __sync_fetch_and_or( &i, mask );

	return 0;
}


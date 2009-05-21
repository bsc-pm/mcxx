
#pragma css task input(t, t2) inout(tt[t][t2][5][5])
void task_a(int t, int t2, int *tt)
{
}

int  main()
{
	int size=1, size2=1;
	int tt[3][5][5][5];

#pragma css start
	task_a(size, size2, tt);
#pragma css finish

}



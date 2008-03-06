#pragma css task input(a) inout(b)
void f(int a, int *b);

#pragma css task input(a) inout(b)
void g(int a, int *b);

void f(int a, int *b) {
	g(a, b);
	if (a > 0) {
		f(a-1, b);
	}
}



// They are the same declaration type
// so it is valid

typedef void f(int*);
typedef void f(int[10]);

typedef int P[10];
typedef void f(P p);

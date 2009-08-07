struct MyComplex {
    private:
        double real;
        double imaginary;
    public:
        MyComplex();
        MyComplex(const MyComplex&);
        MyComplex(double, double);
};

MyComplex operator+(const MyComplex&, const MyComplex &);
MyComplex operator-(const MyComplex&, const MyComplex &);
MyComplex operator*(const MyComplex&, const MyComplex &);

#pragma omp declare reduction type(MyComplex) operator(+, -) identity(constructor(0, 0))
#pragma omp declare reduction type(MyComplex) operator(*) identity(constructor(1, 0))

#define N 100
MyComplex vector[N];

void f(MyComplex x, MyComplex y)
{
    int i;
#pragma omp parallel for reduction(+:x) reduction(*:y)
    for ( i = 0; i < N ; i++ ) 
    {
        x = x + vector[i];
        y = y * vector[i];
    }
}

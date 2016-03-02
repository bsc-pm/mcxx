# User defined reductions for OpenMP (#49)


        <p style="color: red; font-size: 16pt;">This information is outdated and kept here only for historical references. These proposals have been deprecated after OpenMP 4.0 declare reduction syntax.</p>

== Proposal snippet == 

#pragma omp declare reduction [*type*(type-name)] *operator*(op-list) *identity*(const-init-expr| brace-initializer| *constructor*([argument-list])) [*order*(left | right)] [*commutative*]

op-list = op-list | base-operator | function | .member_function
where base-operator is one of the base operators of the language (e.g. +, *,- , ...) 

## Examples of use

### C++ String concatenation


        #!cpp
        #pragma omp declare reduction operator(+) identity(std::string(""))
        
        ...
        
        std::string output;
        #pragma omp parallel reduction(+:output)
        {
             output = "Hello!";
        }
        
        std::cout << output << std::endl;
        

Is there any difference between the example above and the one below?


        #pragma omp declare reduction type(std::string) operator(+) identity(constructor(""))

### C complex reduction

        #!cpp
        typedef struct {
          double real;
          double imaginary;
        } complex_t;
        
        complex_t complex_add (complex_t a, complex_t b);
        complex_t complex_sub (complex_t a, complex_t b);
        complex_t complex_mul (complex_t a, complex_t b);
        
        #pragma omp declare reduction type(complex_t) operator(complex_add,complex_sub) identity({0,0})
        #pragma omp declare reduction type(complex_t) operator(complex_mul) identity({1,0})
        
        ...
        
        complex_t x,y;
        #pragma omp parallel for reduction(complex_add:x) reduction(complex_mul:y)
        for ( i = 0; i < N ; i++ ) {
            x = complex_add(x,vector[i]);
            y = complex_mul(y,vector[i]);
        }

By the way, what about C99 `complex`? Can it be used in an OpenMP `reduction`? Maybe this reduction should be *built-in* like other
arithmetic types.


        #!cpp
        complex float c_v[100];
        complex float c_sum = 0.0;
        
        #pragma omp parallel for reduction(+:c_sum)
        for (int i = 0; i < 100; i++)
        {
          c_sum += c_v[i];
        }

### C++ complex reduction

        #!cpp
        class Complex {
        private:
            double real;
            double imaginary;
        public:
            Complex(double, double);
        
            Complex operator+ (const Complex &);
            Complex operator- (const Complex &);
            Complex operator* (const Complex &);
        };
        
        #pragma omp type(Complex) operator(+) identity(constructor(0,0))
        #pragma omp type(Complex) operator(-) identity(constructor(0,0))
        #pragma omp type(Complex) operator(*) identity(constructor(1,0))
        
        Complex x,y;
        #pragma omp parallel for reduction(+:x) reduction(*:y)
        for ( i = 0; i < N ; i++ ) {
            x += vector[i];
            y *= vector[i];
        }

### Using members for reduction

        #!cpp
        Class A {
        ...
            redux(const A&);
        }
        
        #pragma omp declare reduction type(A) operation(.redux) init(...)
        
        A a;
        #pragma omp for reduction(redux:a)
        for ( i = 0; i < N; i++)
           a.redux(vectorA[i]);

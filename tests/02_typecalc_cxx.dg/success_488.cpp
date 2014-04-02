/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

enum E { V = 3 };

void f1(int x) 
{ 
    typedef int Int;

    typedef __typeof__(x + V) Int; 
    typedef __typeof__(x - V) Int; 
    typedef __typeof__(V + x) Int; 
    typedef __typeof__(V - x) Int; 
    typedef __typeof__(x * V) Int; 
    typedef __typeof__(V * x) Int; 
    typedef __typeof__(x / V) Int; 
    typedef __typeof__(V / x) Int; 
    typedef __typeof__(x % V) Int; 
    typedef __typeof__(V % x) Int; 
    typedef __typeof__(x << V) Int; 
    typedef __typeof__(x >> V) Int; 
    typedef __typeof__(x += V) Int; 
    typedef __typeof__(x -= V) Int; 
    typedef __typeof__(x *= V) Int; 
    typedef __typeof__(x /= V) Int; 
    typedef __typeof__(x %= V) Int; 
    typedef __typeof__(x <<= V) Int; 
    typedef __typeof__(x >>= V) Int; 
    typedef __typeof__( +V) Int; 
    typedef __typeof__( -V) Int; 
}

void f2(long x) 
{ 
    typedef long Long;
    typedef int Int;
    typedef __typeof__(x + V) Long; 
    typedef __typeof__(x - V) Long; 
    typedef __typeof__(V + x) Long; 
    typedef __typeof__(V - x) Long; 
    typedef __typeof__(x * V) Long; 
    typedef __typeof__(V * x) Long; 
    typedef __typeof__(x / V) Long; 
    typedef __typeof__(V / x) Long; 
    typedef __typeof__(x % V) Long; 
    typedef __typeof__(V % x) Long; 
    typedef __typeof__(x << V) Long; 
    typedef __typeof__(V << x) Int;
    typedef __typeof__(x >> V) Long; 
    typedef __typeof__(V >> x) Int; 
    typedef __typeof__(x += V) Long; 
    typedef __typeof__(x -= V) Long; 
    typedef __typeof__(x *= V) Long; 
    typedef __typeof__(x /= V) Long; 
    typedef __typeof__(x %= V) Long; 
    typedef __typeof__(x <<= V) Long; 
    typedef __typeof__(x >>= V) Long; 
}

void f3(char x) 
{ 
    typedef int Int;
    typedef char Char;

    typedef __typeof__(x + V) Int; 
    typedef __typeof__(x - V) Int; 
    typedef __typeof__(V + x) Int; 
    typedef __typeof__(V - x) Int; 
    typedef __typeof__(x * V) Int; 
    typedef __typeof__(V * x) Int; 
    typedef __typeof__(x / V) Int; 
    typedef __typeof__(V / x) Int; 
    typedef __typeof__(x % V) Int; 
    typedef __typeof__(V % x) Int; 
    typedef __typeof__(x << V) Int; 
    typedef __typeof__(V << x) Int;
    typedef __typeof__(x >> V) Int; 
    typedef __typeof__(V >> x) Int; 
    typedef __typeof__(x += V) Char; 
    typedef __typeof__(x -= V) Char; 
    typedef __typeof__(x *= V) Char; 
    typedef __typeof__(x /= V) Char; 
    typedef __typeof__(x %= V) Char; 
    typedef __typeof__(x <<= V) Char; 
    typedef __typeof__(x >>= V) Char; 
}

void f4(float x) 
{ 
    typedef float Float;
    typedef __typeof__(x + V) Float; 
    typedef __typeof__(x - V) Float; 
    typedef __typeof__(V + x) Float; 
    typedef __typeof__(V - x) Float; 
    typedef __typeof__(x * V) Float; 
    typedef __typeof__(V * x) Float; 
    typedef __typeof__(x / V) Float; 
    typedef __typeof__(V / x) Float; 
    typedef __typeof__(x += V) Float; 
    typedef __typeof__(x -= V) Float; 
    typedef __typeof__(x *= V) Float; 
    typedef __typeof__(x /= V) Float; 
}

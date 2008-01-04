void f1(float*);
void f2(int*);
void f3(double*);
void f4(bool*);

struct A
{
    // f1
    float* operator+(A&);
    float* operator-(A&);
    float* operator*(A&);
    float* operator/(A&);
    float* operator%(A&);

    float *operator!();
    float* operator&&(A&);
    float* operator||(A&);

    float* operator~();
    float* operator|(A&);
    float* operator&(A&);
    float* operator^(A&);

    float* operator<<(A&);
    float* operator>>(A&);

    float* operator+=(A&);
    float* operator-=(A&);
    float* operator*=(A&);
    float* operator/=(A&);
    float* operator%=(A&);
    float* operator|=(A&);
    float* operator&=(A&);
    float* operator^=(A&);
    float* operator<<=(A&);
    float* operator>>=(A&);

    // f2
    int* operator+(const A&) const;
    int* operator*(const A&) const;
    int* operator/(const A&) const;
    int* operator-(const A&) const;
    int* operator%(const A&) const;

    int *operator!() const;
    int* operator&&(const A&) const;
    int* operator||(const A&) const;

    int* operator~() const;
    int* operator|(const A&) const;
    int* operator&(const A&) const;
    int* operator^(const A&) const;

    int* operator<<(const A&) const;
    int* operator>>(const A&) const;

    int* operator+=(const A&) const;
    int* operator-=(const A&) const;
    int* operator*=(const A&) const;
    int* operator/=(const A&) const;
    int* operator%=(const A&) const;
    int* operator|=(const A&) const;
    int* operator&=(const A&) const;
    int* operator^=(const A&) const;
    int* operator<<=(const A&) const;
    int* operator>>=(const A&) const;

    // f3
    double* operator+(A&) const;
    double* operator*(A&) const;
    double* operator/(A&) const;
    double* operator-(A&) const;
    double* operator%(A&) const;

    double* operator&&(A&) const;
    double* operator||(A&) const;

    double* operator|(A&) const;
    double* operator&(A&) const;
    double* operator^(A&) const;

    double* operator<<(A&) const;
    double* operator>>(A&) const;

    double* operator+=(A&) const;
    double* operator-=(A&) const;
    double* operator*=(A&) const;
    double* operator/=(A&) const;
    double* operator%=(A&) const;
    double* operator|=(A&) const;
    double* operator&=(A&) const;
    double* operator^=(A&) const;
    double* operator<<=(A&) const;
    double* operator>>=(A&) const;
    
    // f4
    bool* operator+(const A&);
    bool* operator*(const A&);
    bool* operator/(const A&);
    bool* operator-(const A&);
    bool* operator%(const A&);

    bool* operator&&(const A&);
    bool* operator||(const A&);

    bool* operator|(const A&);
    bool* operator&(const A&);
    bool* operator^(const A&);

    bool* operator<<(const A&);
    bool* operator>>(const A&);

    bool* operator+=(const A&);
    bool* operator-=(const A&);
    bool* operator*=(const A&);
    bool* operator/=(const A&);
    bool* operator%=(const A&);
    bool* operator|=(const A&);
    bool* operator&=(const A&);
    bool* operator^=(const A&);
    bool* operator<<=(const A&);
    bool* operator>>=(const A&);
};


void g()
{
    A a, b;

    // f1 ------------
    f1(a + b);
    f1(a - b);
    f1(a * b);
    f1(a / b);
    f1(a % b);

    f1(!a);
    f1(a && b);
    f1(a || b);

    f1(~a);
    f1(a & b);
    f1(a | b);
    f1(a ^ b);

    f1(a << b);
    f1(a >> b);

    f1(a += b);
    f1(a -= b);
    f1(a *= b);
    f1(a /= b);
    f1(a %= b);
    f1(a |= b);
    f1(a &= b);
    f1(a ^= b);
    f1(a <<= b);
    f1(a >>= b);

    // f2 -----------
    const A &c_a = a, 
          &c_b = b;

    f2(c_a + c_b);
    f2(c_a - c_b);
    f2(c_a * c_b);
    f2(c_a / c_b);
    f2(c_a % c_b);

    f2(!c_a);
    f2(c_a && c_b);
    f2(c_a || c_b);

    f2(~c_a);
    f2(c_a & c_b);
    f2(c_a | c_b);
    f2(c_a ^ c_b);

    f2(c_a << c_b);
    f2(c_a >> c_b);

    f2(c_a += c_b);
    f2(c_a -= c_b);
    f2(c_a *= c_b);
    f2(c_a /= c_b);
    f2(c_a %= c_b);
    f2(c_a |= c_b);
    f2(c_a &= c_b);
    f2(c_a ^= c_b);
    f2(c_a <<= c_b);
    f2(c_a >>= c_b);

    // f3 -------------
    f3(c_a + b);
    f3(c_a - b);
    f3(c_a * b);
    f3(c_a / b);
    f3(c_a % b);

    f3(c_a && b);
    f3(c_a || b);

    f3(c_a & b);
    f3(c_a | b);
    f3(c_a ^ b);

    f3(c_a << b);
    f3(c_a >> b);

    f3(c_a += b);
    f3(c_a -= b);
    f3(c_a *= b);
    f3(c_a /= b);
    f3(c_a %= b);
    f3(c_a |= b);
    f3(c_a &= b);
    f3(c_a ^= b);
    f3(c_a <<= b);
    f3(c_a >>= b);
    
    // f4 -------------
    f4(a + c_b);
    f4(a - c_b);
    f4(a * c_b);
    f4(a / c_b);
    f4(a % c_b);

    f4(a && c_b);
    f4(a || c_b);

    f4(a & c_b);
    f4(a | c_b);
    f4(a ^ c_b);

    f4(a << c_b);
    f4(a >> c_b);

    f4(a += c_b);
    f4(a -= c_b);
    f4(a *= c_b);
    f4(a /= c_b);
    f4(a %= c_b);
    f4(a |= c_b);
    f4(a &= c_b);
    f4(a ^= c_b);
    f4(a <<= c_b);
    f4(a >>= c_b);
}

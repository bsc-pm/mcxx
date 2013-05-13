/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  See AUTHORS file in the top level directory for information
  regarding developers and contributors.
  
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 3 of the License, or (at your option) any later version.
  
  Mercurium C/C++ source-to-source compiler is distributed in the hope
  that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the GNU Lesser General Public License for more
  details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with Mercurium C/C++ source-to-source compiler; if
  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
  Cambridge, MA 02139, USA.
--------------------------------------------------------------------*/



/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

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

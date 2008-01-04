/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2008 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
void f1(float*);
void f2(int*);
void f3(double*);
void f4(bool*);

struct A
{
};

// f1
float* operator+(A&, A&);
float* operator-(A&, A&);
float* operator*(A&, A&);
float* operator/(A&, A&);
float* operator%(A&, A&);

float *operator!(A&);
float* operator&&(A&, A&);
float* operator||(A&, A&);

float* operator~(A&);
float* operator|(A&, A&);
float* operator&(A&, A&);
float* operator^(A&, A&);

float* operator<<(A&, A&);
float* operator>>(A&, A&);

float* operator+=(A&, A&);
float* operator-=(A&, A&);
float* operator*=(A&, A&);
float* operator/=(A&, A&);
float* operator%=(A&, A&);
float* operator|=(A&, A&);
float* operator&=(A&, A&);
float* operator^=(A&, A&);
float* operator<<=(A&, A&);
float* operator>>=(A&, A&);

// f2
int* operator+(const A&, const A&);
int* operator-(const A&, const A&);
int* operator*(const A&, const A&);
int* operator/(const A&, const A&);
int* operator%(const A&, const A&);

int *operator!(const A&);
int* operator&&(const A&, const A&);
int* operator||(const A&, const A&);

int* operator~(const A&);
int* operator|(const A&, const A&);
int* operator&(const A&, const A&);
int* operator^(const A&, const A&);

int* operator<<(const A&, const A&);
int* operator>>(const A&, const A&);

int* operator+=(const A&, const A&);
int* operator-=(const A&, const A&);
int* operator*=(const A&, const A&);
int* operator/=(const A&, const A&);
int* operator%=(const A&, const A&);
int* operator|=(const A&, const A&);
int* operator&=(const A&, const A&);
int* operator^=(const A&, const A&);
int* operator<<=(const A&, const A&);
int* operator>>=(const A&, const A&);

// f3
double* operator+(const A&, A&);
double* operator-(const A&, A&);
double* operator*(const A&, A&);
double* operator/(const A&, A&);
double* operator%(const A&, A&);

double* operator&&(const A&, A&);
double* operator||(const A&, A&);

double* operator|(const A&, A&);
double* operator&(const A&, A&);
double* operator^(const A&, A&);

double* operator<<(const A&, A&);
double* operator>>(const A&, A&);

double* operator+=(const A&, A&);
double* operator-=(const A&, A&);
double* operator*=(const A&, A&);
double* operator/=(const A&, A&);
double* operator%=(const A&, A&);
double* operator|=(const A&, A&);
double* operator&=(const A&, A&);
double* operator^=(const A&, A&);
double* operator<<=(const A&, A&);
double* operator>>=(const A&, A&);

// f4
bool* operator+(A&, const A&);
bool* operator-(A&, const A&);
bool* operator*(A&, const A&);
bool* operator/(A&, const A&);
bool* operator%(A&, const A&);

bool* operator&&(A&, const A&);
bool* operator||(A&, const A&);

bool* operator|(A&, const A&);
bool* operator&(A&, const A&);
bool* operator^(A&, const A&);

bool* operator<<(A&, const A&);
bool* operator>>(A&, const A&);

bool* operator+=(A&, const A&);
bool* operator-=(A&, const A&);
bool* operator*=(A&, const A&);
bool* operator/=(A&, const A&);
bool* operator%=(A&, const A&);
bool* operator|=(A&, const A&);
bool* operator&=(A&, const A&);
bool* operator^=(A&, const A&);
bool* operator<<=(A&, const A&);
bool* operator>>=(A&, const A&);

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

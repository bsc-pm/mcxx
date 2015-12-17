/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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
test_generator=config/mercurium-extensions
compile_versions="cxx03 cxx11"
test_CXXFLAGS="--env=linux-x86_64 --enable-ms-builtins --enable-intel-vector-types"
test_CXXFLAGS_cxx11="-std=c++11 --pp=off"
</testinfo>
*/

typedef struct __declspec(align(16)) __declspec(intrin_type) __m128 {
    float               m128_f32[4];
} __m128;

typedef struct __declspec(align(16)) __declspec(intrin_type) __m128d {
    double              m128d_f64[2];
} __m128d;

typedef union  __declspec(align(16)) __declspec(intrin_type) __m128i {
     __int64             m128i_gcc_compatibility[2];
    __int8              m128i_i8[16];
    __int16             m128i_i16[8];
    __int32             m128i_i32[4];
    __int64             m128i_i64[2];
    unsigned __int8     m128i_u8[16];
    unsigned __int16    m128i_u16[8];
    unsigned __int32    m128i_u32[4];
    unsigned __int64    m128i_u64[2];
    char c[16];
} __m128i;

typedef union  __declspec(align(32)) __declspec(intrin_type) __m256 {
    float m256_f32[8];
} __m256;

typedef struct __declspec(align(32)) __declspec(intrin_type) __m256d {
    double m256d_f64[4];
} __m256d;

typedef union  __declspec(align(32)) __declspec(intrin_type) __m256i {
    __int64             m256i_gcc_compatibility[4];
    __int8              m256i_i8[32];
    __int16             m256i_i16[16];
    __int32             m256i_i32[8];
    __int64             m256i_i64[4];
    unsigned __int8     m256i_u8[32];
    unsigned __int16    m256i_u16[16];
    unsigned __int32    m256i_u32[8];
    unsigned __int64    m256i_u64[4];
} __m256i;

typedef union __declspec(align(64)) __declspec(intrin_type) __m512 {
    float       __m512_f32[16];
} __m512;

typedef union __declspec(align(64)) __declspec(intrin_type) __m512d {
    double      __m512d_f64[8];
} __m512d;

typedef union __declspec(align(64)) __declspec(intrin_type) __m512i {
    int         __m512i_i32[16];
} __m512i;

void g128i(__m128i);
void g128(__m128);
void g128d(__m128d);

void g256i(__m256i);
void g256(__m256);
void g256d(__m256d);

void g512i(__m512i);
void g512(__m512);
void g512d(__m512d);

void f(void)
{
    // 128
    __m128 v128;
    __m128i v128i;
    __m128d v128d;

    g128(v128);
    g128(v128d);
    g128(v128i);

    g128i(v128);
    g128i(v128i);
    g128i(v128d);

    g128d(v128);
    g128d(v128i);
    g128d(v128d);

    // 256
    __m256 v256;
    __m256i v256i;
    __m256d v256d;

    g256(v256);
    g256(v256d);
    g256(v256i);

    g256i(v256);
    g256i(v256i);
    g256i(v256d);

    g256d(v256);
    g256d(v256i);
    g256d(v256d);

    // 512
    __m512 v512;
    __m512i v512i;
    __m512d v512d;

    g512(v512);
    g512(v512d);
    g512(v512i);

    g512i(v512);
    g512i(v512i);
    g512i(v512d);

    g512d(v512);
    g512d(v512i);
    g512d(v512d);
}

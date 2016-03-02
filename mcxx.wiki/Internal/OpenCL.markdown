# OpenCL support in Mercurium

## 6.1.1. Built-in Scalar Data Types

OpenCL defines fixed sizes for the usual C99 scalar types similar to a I32LP64 environment with some minor changes.

New types are

 * `bool` (C99 already has `_Bool`)
 * `half` this is a 16-bit floating point (in addition to `float` of `double` of 32 and 64 bit respectively)
 * `size_t` this is actually defined in `stddef.h`

Most language types have equivalents of the form `cl_`typename for the purpose of the API, e.g. `int` → `cl_int`

### half data type

It is a rather limited data type which is only valid for buffers i.e. `half*`.

## 6.1.2. Builtin vector types

|`char`n|`cl_char`n|
|-------|----------|
|`uchar`n|`cl_uchar`n|
|`short`n|`cl_short`n|
|`ushort`n|`cl_ushort`n|
|`int`n|`cl_int`n|
|`uint`n|`cl_uint`n|
|`long`n|`cl_long`n|
|`ulong`n|`cl_ulong`n|
|`float`n|`cl_float`n|

The compiler already has some notion of vector types but modelled after GCC vectors. Not sure how will they fit in OpenCL's.

## 6.1.3. Other built-in data types

|`image2d_t`|
|-----------|
|`image3d_t`|
|`sampler_t`|
|`event_t`|

It seems these types are only useable by means of APIs and no language constructs are required for those.

## 6.1.6. Vector literals


_vector_literal_:
 `(` _vector-type-name_ `)` `(` _expression-list_ `)`

I think that a GCC extension is already covering this case or a very similar one.

## 6.1.7. Vector components

_postfix-expression_:
  _postfix-expression_ . _vector-component-selector_

_vector-component-selector_: 
 _vector-component-selector4_ 

 _vector-component-selector3_ 

 _vector-component-selector2_ 

 _vector-component-selector1_ 

 _vector-numeric-selector_ 

 _vector-suffix-selector_

_vector-component-selector4_: 
 _vector-component-selector3_ _vector-component-item_
_vector-component-selector3_: 
 _vector-component-selector2_ _vector-component-item_ 
_vector-component-selector2_: 
 _vector-component-selector1_ _vector-component-item_
_vector-component-selector_: 
 _vector-component-item_

_vector-component-item_: 
 `x` | `y` | `z` | `w`

_vector-numeric-selector_ : 
 `s`_vector-numeric-selector-item_

_vector-numeric-selector-item_: 
 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | a | A | b | B | c | C | d | D | e | E | f | F

_vector-suffix-selector_:
 odd | even | lo | hi
 
I am against adding these rules in the grammar but instead enforce the syntax in the typechecking of the already usual _member access_ of _postfix-expression_.

## 6.2 Conversions

Implicit or explicit conversions are not allowed between vector types.

A scalar expression can be expanded to a vector of the scalar type.

### 6.2.3 Explicit conversions

From scalar to vector.

        destType convert_destType<_sat><_roundingMode> (sourceType)

From vector to vector

        destTypen convert_destTypen<_sat><_roundingMode> (sourceTypen)

I think these can be signed in as builtins for each supported vector type.

__roundingMode_ :
 `_rte` 

 `_rtz` 

 `_rtp` 

 `_rtn`

### 6.2.4 Reinterpreting types


        type as_type ( almost-any-scalar-type )
        typen as_typen ( almost-any-scalar-type )

_almost-any-scalar-type_ includes all scalars types but `bool`, `half` and `void`.

These can be signed as builtins as well

## 6.3 Operators

We should extend operators to support vectors as well. Some work is already there to support GCC vectors. We should see how to extend it to support OpenCL vectors. The full support is similar to Fortran 90 vector support where scalars can be widened to vectors before operating with vectors.

## 6.5 Address space qualifiers

_decl_specifier_ : 
 _address_space_qualifier_

_address_space_qualifier_ :
 `__global` 

 `global` 

 `__local` 

 `local` 

 `__private` 

 `private` 

 `__constant` 

 `constant`

## 6.6 Access qualifiers

_decl_specifier_ :
 _access_qualifier_

_access_qualifier_:
 `__read_only` 

 `read_only` 

 `__write_only` 

 `write_only` 

## 6.7 Function qualifiers

''decl_specifier' :
 _function_qualifier_

_function-qualifier_ :
  `__kernel` 

  `kernel`


## 6.9 Preprocessor directives and macros

Macros

|`__OPENCL_VERSION__`|110|
|--------------------|---|
|`CL_VERSION_1_0`|100|
|`CL_VERSION_1_1`|110|
|`__ENDIAN_LITTLE__`|I think this is already in some of the Linux headers|
|`__IMAGE_SUPPORT__`|If image support is available|
|`__FAST_RELAXED_MATH__`| |

Pragma interface


        #pragma OPENCL FP_CONTRACT on-off-switch
          on-off-switch: one of ON OFF DEFAULT
        #pragma OPENCL EXTENSION extensionname : behavior
        #pragma OPENCL EXTENSION all : behavior
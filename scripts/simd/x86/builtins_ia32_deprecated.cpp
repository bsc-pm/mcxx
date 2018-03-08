//
// Generator of src/frontend/cxx-gccbuiltins-ia32-deprecated.h for gcc
//
// Use make generate_builtins_ia32 (at the tp level) to compile this file
//

#include <iostream>
#include <sstream>
#include "builtins-common.hpp"

// --------------------------------------------
// Specific generators for IA32
// --------------------------------------------

#define GENERATE_VECTOR(N, T) \
template <>\
struct generate_type<__attribute__((vector_size(N))) T>\
{\
    static const int size = N;\
    typedef T element_type;\
\
    static std::string g() \
    {\
        std::stringstream ss;\
\
        ss << "get_vector_type_by_bytes(" << generate_type<element_type>::g() << ", " << N << ")";\
\
        return ss.str();\
    }\
};\

#define GENERATE_MANY(T) \
   GENERATE_VECTOR(8, T) \
   GENERATE_VECTOR(16, T) \
   GENERATE_VECTOR(32, T) \
   GENERATE_VECTOR(64, T)


GENERATE_MANY(int)
GENERATE_MANY(signed char)
GENERATE_MANY(char)
GENERATE_MANY(short)
GENERATE_MANY(long)
GENERATE_MANY(long long)

GENERATE_MANY(unsigned int)
GENERATE_MANY(unsigned char)
GENERATE_MANY(unsigned short)
GENERATE_MANY(unsigned long)
GENERATE_MANY(unsigned long long)

GENERATE_MANY(float)
GENERATE_MANY(double)

// --------------------------------------------
// End of specific generators for IA32
// --------------------------------------------

#define END

#define VECTOR_INTRINSICS_LIST \
\
/* pcommit was removed in GCC 6 */ \
VECTOR_INTRIN(__builtin_ia32_pcommit) \
\
/* these intrinsics were removed in GCC 7 */ \
VECTOR_INTRIN(__builtin_ia32_kmov16) \
VECTOR_INTRIN(__builtin_ia32_vpcomneb) \
VECTOR_INTRIN(__builtin_ia32_vpcomned) \
VECTOR_INTRIN(__builtin_ia32_vpcomneq) \
VECTOR_INTRIN(__builtin_ia32_vpcomneub) \
VECTOR_INTRIN(__builtin_ia32_vpcomneud) \
VECTOR_INTRIN(__builtin_ia32_vpcomneuq) \
VECTOR_INTRIN(__builtin_ia32_vpcomneuw) \
VECTOR_INTRIN(__builtin_ia32_vpcomnew) \
\
/* The prototype of these builtins was changed in GCC 7*/ \
OVERLOADED_VECTOR_INTRIN(__builtin_ia32_loaddqudi128_mask) \
OVERLOADED_VECTOR_INTRIN(__builtin_ia32_loaddqudi256_mask) \
OVERLOADED_VECTOR_INTRIN(__builtin_ia32_loaddqudi512_mask) \
OVERLOADED_VECTOR_INTRIN(__builtin_ia32_loaddquhi128_mask) \
OVERLOADED_VECTOR_INTRIN(__builtin_ia32_loaddquhi256_mask) \
OVERLOADED_VECTOR_INTRIN(__builtin_ia32_loaddquhi512_mask) \
OVERLOADED_VECTOR_INTRIN(__builtin_ia32_loaddquqi128_mask) \
OVERLOADED_VECTOR_INTRIN(__builtin_ia32_loaddquqi256_mask) \
OVERLOADED_VECTOR_INTRIN(__builtin_ia32_loaddquqi512_mask) \
OVERLOADED_VECTOR_INTRIN(__builtin_ia32_loaddqusi128_mask) \
OVERLOADED_VECTOR_INTRIN(__builtin_ia32_loaddqusi256_mask) \
OVERLOADED_VECTOR_INTRIN(__builtin_ia32_loaddqusi512_mask) \
OVERLOADED_VECTOR_INTRIN(__builtin_ia32_loadupd128_mask) \
OVERLOADED_VECTOR_INTRIN(__builtin_ia32_loadupd256_mask) \
OVERLOADED_VECTOR_INTRIN(__builtin_ia32_loadupd512_mask) \
OVERLOADED_VECTOR_INTRIN(__builtin_ia32_loadups128_mask) \
OVERLOADED_VECTOR_INTRIN(__builtin_ia32_loadups256_mask) \
OVERLOADED_VECTOR_INTRIN(__builtin_ia32_loadups512_mask) \
OVERLOADED_VECTOR_INTRIN(__builtin_ia32_storedqudi128_mask) \
OVERLOADED_VECTOR_INTRIN(__builtin_ia32_storedqudi256_mask) \
OVERLOADED_VECTOR_INTRIN(__builtin_ia32_storedqudi512_mask) \
OVERLOADED_VECTOR_INTRIN(__builtin_ia32_storedquhi128_mask) \
OVERLOADED_VECTOR_INTRIN(__builtin_ia32_storedquhi256_mask) \
OVERLOADED_VECTOR_INTRIN(__builtin_ia32_storedquhi512_mask) \
OVERLOADED_VECTOR_INTRIN(__builtin_ia32_storedquqi128_mask) \
OVERLOADED_VECTOR_INTRIN(__builtin_ia32_storedquqi256_mask) \
OVERLOADED_VECTOR_INTRIN(__builtin_ia32_storedquqi512_mask) \
OVERLOADED_VECTOR_INTRIN(__builtin_ia32_storedqusi128_mask) \
OVERLOADED_VECTOR_INTRIN(__builtin_ia32_storedqusi256_mask) \
OVERLOADED_VECTOR_INTRIN(__builtin_ia32_storedqusi512_mask) \
OVERLOADED_VECTOR_INTRIN(__builtin_ia32_storeupd128_mask) \
OVERLOADED_VECTOR_INTRIN(__builtin_ia32_storeupd256_mask) \
OVERLOADED_VECTOR_INTRIN(__builtin_ia32_storeupd512_mask) \
OVERLOADED_VECTOR_INTRIN(__builtin_ia32_storeups128_mask) \
OVERLOADED_VECTOR_INTRIN(__builtin_ia32_storeups256_mask) \
OVERLOADED_VECTOR_INTRIN(__builtin_ia32_storeups512_mask) \
END

int main(int, char**)
{
#define VECTOR_INTRIN(X) \
    f<__typeof__(X)>(#X);

#define OVERLOADED_VECTOR_INTRIN(X) \
    std::cout << "if (IS_CXX_LANGUAGE) {\n";  \
        VECTOR_INTRIN(X) \
    std::cout << "}\n";

    VECTOR_INTRINSICS_LIST

#undef VECTOR_INTRIN
#undef OVERLOADED_VECTOR_INTRIN
}

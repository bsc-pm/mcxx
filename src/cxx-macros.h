#ifndef CXX_MACROS_H
#define CXX_MACROS_H

// Some useful macros
#ifndef __GNUC__
    #define __attribute__(x)
#endif

// Some gcc-isms
#ifdef __GNUC__
  #if __GNUC__ == 3 
     #define NORETURN __attribute__((noreturn))

     #if __GNUC_MINOR__ >= 4
         #define WARN_UNUSED __attribute__((warn_unused_result))
     #else
         #define WARN_UNUSED
     #endif
  #elif __GNUC__ == 4
     #define NORETURN __attribute__((noreturn))
	 #define WARN_UNUSED __attribute__((warn_unused_result))
  #elif __GNUC__ == 2
     #error "This code will not compile with GCC 2"
  #endif
#else
  #define NORETURN
  #define WARN_UNUSED
#endif

#ifdef __cplusplus
  #define MCXX_BEGIN_DECLS extern "C" { 
  #define MCXX_END_DECLS }
#else
  #define MCXX_BEGIN_DECLS
  #define MCXX_END_DECLS
#endif

#define BITMAP_TEST(x, b) (((x) & (b)) == (b))

#endif // CXX_MACROS_H

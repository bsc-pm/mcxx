#ifndef LIBMCXX_COMMON_H
#define LIBMCXX_COMMON_H

#ifdef WIN32_BUILD
  #ifdef LIBMCXX_DLL_EXPORT
    #define LIBMCXX_EXTERN extern __declspec(dllexport)
  #else
    #define LIBMCXX_EXTERN extern __declspec(dllimport)
  #endif
#else
  #define LIBMCXX_EXTERN extern
#endif

#endif

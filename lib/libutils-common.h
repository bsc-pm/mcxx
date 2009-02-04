#ifndef LIBUTILS_COMMON_H
#define LIBUTILS_COMMON_H

#ifdef _WIN32
  #ifdef LIBUTILS_DLL_EXPORT
    #define LIBUTILS_EXTERN extern __declspec(dllexport)
  #else
    #define LIBUTILS_EXTERN extern __declspec(dllimport)
  #endif
#else
  #define LIBUTILS_EXTERN extern
#endif


#endif

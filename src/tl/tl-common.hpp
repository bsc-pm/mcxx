#ifndef TL_COMMON_HPP
#define TL_COMMON_HPP

#ifdef _WIN32
  #ifdef LIBTL_DLL_EXPORT
    #define LIBTL_EXTERN extern __declspec(dllexport)
    #define LIBTL_CLASS __declspec(dllexport)
  #else
    #define LIBTL_EXTERN extern __declspec(dllimport)
    #define LIBTL_CLASS __declspec(dllimport)
  #endif
  #define LIBTL_ALWAYS_EXPORT __declspec(dllexport)
#else
  #define LIBTL_EXTERN extern
  #define LIBTL_CLASS
  #define LIBTL_ALWAYS_EXPORT 
#endif

#endif // TL_COMMON_HPP

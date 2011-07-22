#ifndef CXX_DRIVER_FORTRAN_H
#define CXX_DRIVER_FORTRAN_H


// This function states to the driver that we are going to use the module 'module_name'
// it returns the path of the mf03 specific module and ensures that during native compilation
// the native modules will be available as well
void driver_fortran_retrieve_module(const char* module_name, const char **mf03_filename);

// This function states to the driver that we are going to create the module 'module_name'
// it returns the path where the mf03 specific module will be created
// The driver will ensure that the native module is properly wrapped along with mf03_filename
void driver_fortran_register_module(const char* module_name, const char **mf03_filename);

// This function is called by the driver after the native compilation of a Fortran file
void driver_fortran_wrap_all_modules(void);

// This function is called by the driver if native compilation is not actually performed
void driver_fortran_discard_all_modules(void);

// This function hides all wrap modules, lest they were found by the native compiler
void driver_fortran_hide_mercurium_modules(void);

// This function restores all wrap modules, for subsequent uses
void driver_fortran_restore_mercurium_modules(void);

#endif // CXX_DRIVER_FORTRAN_H

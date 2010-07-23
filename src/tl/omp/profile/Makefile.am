include ../../Makefile.phases

lib_LTLIBRARIES = libtlomp-profile.la

tl_omp_flags = -Wall $(tl_cflags) -I $(srcdir)/../core -I $(srcdir)/../nanos-common

END=

libtlomp_profile_la_SOURCES = \
                              omp-profile.cpp \
                              omp-profile.hpp \
                              omp-closure.cpp \
                              $(END)

libtlomp_profile_la_CFLAGS=$(tl_omp_flags) 
libtlomp_profile_la_CXXFLAGS=$(tl_omp_flags) 
libtlomp_profile_la_LDFLAGS=$(tl_ldflags)
libtlomp_profile_la_LIBADD=$(tl_libadd) ../core/libtlomp-core.la

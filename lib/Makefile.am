noinst_PROGRAMS = 

if TPP_BUILDING
noinst_PROGRAMS += tpp
tpp_SOURCES = \
              tpp.c
endif

noinst_LTLIBRARIES = libmcxx-utils.la libmcxx-mcfg.la libmcxx-extstruct.la
EXTRA_LTLIBRARIES=libmcxx-getopt.la

libmcxx_getopt_la_CFLAGS= -Wall
libmcxx_getopt_la_SOURCES = \
                           getopt.c \
                           getopt1.c \
                           getopt.h

libmcxx_utils_la_CFLAGS = -Wall -I $(srcdir)/../gc/include
libmcxx_utils_la_SOURCES = \
                          iterator.c \
                          iterator.h \
                          list.c \
                          list.h \
                          list_iterator.c \
                          list_iterator.h \
                          mem_ctl.h \
                          mem_ctl.c \
                          s_types.h \
                          hash.c \
                          hash.h \
                          hash_iterator.h \
                          hash_iterator.c \
                          stack.h \
						  mcxx_refcount.c \
						  mcxx_refcount.h


libmcxx_mcfg_la_CFLAGS = -Wall -I $(srcdir)/../gc/include
libmcxx_mcfg_la_SOURCES = \
                          mcfg.h \
                          mcfg.c

libmcxx_extstruct_la_CFLAGS = -Wall -I $(srcdir)/../gc/include
libmcxx_extstruct_la_SOURCES = \
                              extstruct.h \
                              extstruct.c 


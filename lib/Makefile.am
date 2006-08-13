noinst_LTLIBRARIES = libmcxx-utils.la libmcxx-mcfg.la
EXTRA_LTLIBRARIES=libmcxx-getopt.la

libmcxx_getopt_la_SOURCES = getopt.c \
						   getopt1.c \
						   getopt.h
libmcxx_getopt_la_CFLAGS=

libmcxx_utils_la_SOURCES = iterator.c \
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
						  stack.h
libmcxx_utils_la_CFLAGS = -I $(srcdir)/../gc/include

libmcxx_mcfg_la_SOURCES = mcfg.h \
                          mcfg.c
libmcxx_mcfg_la_CFLAGS = -I $(srcdir)/../gc/include

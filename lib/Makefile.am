noinst_PROGRAMS = tpp

CC_FOR_BUILD=@CC_FOR_BUILD@

# tpp
tpp_SOURCES = tpp.c

tpp$(EXEEXT): tpp.o
	$(CC_FOR_BUILD) -o $@ $+ 

tpp.o : tpp.c
	$(CC_FOR_BUILD) -c -o $@ $(srcdir)/tpp.c $(DEFAULT_INCLUDES) \
	         -DPACKAGE=\"$(PACKAGE)\" -DVERSION=\"$(PACKAGE_VERSION)\"
#end of tpp


lib_LTLIBRARIES = libmcxx-utils.la libmcxx-mcfg.la libmcxx-extstruct.la

if WINDOWS_BUILD
no_undefined=-no-undefined
else
no_undefined=
endif

END=

libmcxx_utils_la_CFLAGS = -Wall -DLIBUTILS_DLL_EXPORT
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
						  mcxx_refcount.h \
						  uniquestr.h \
						  char_trie.c \
                          libutils-common.h \
                          $(END)

libmcxx_utils_la_LDFLAGS= -avoid-version $(no_undefined)


libmcxx_mcfg_la_CFLAGS = -Wall -DLIBMCFG_DLL_EXPORT
libmcxx_mcfg_la_SOURCES = \
                          mcfg.h \
                          mcfg.c
libmcxx_mcfg_la_LDFLAGS= -avoid-version $(no_undefined)

libmcxx_extstruct_la_CFLAGS = -Wall -DLIBEXTSTRUCT_DLL_EXPORT
libmcxx_extstruct_la_SOURCES = \
                              extstruct.h \
                              extstruct.c 
libmcxx_extstruct_la_LDFLAGS=-avoid-version $(no_undefined)
libmcxx_extstruct_la_LIBADD=libmcxx-utils.la

dist-hook: tpp$(EXEEXT)

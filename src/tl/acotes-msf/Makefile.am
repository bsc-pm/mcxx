#   $Id$

include ../Makefile.phases

lib_LTLIBRARIES = libtlacotes.la libtlacotes-output.la

COMMON_CFLAGS= -Wall $(tl_cflags)

libtlacotes_la_CFLAGS = $(COMMON_CFLAGS)
libtlacotes_la_CXXFLAGS = $(COMMON_CFLAGS)
libtlacotes_la_LDFLAGS = $(tl_ldflags)
libtlacotes_la_LIBADD = $(tl_libadd) libtlacotes-output.la

libtlacotes_la_SOURCES = \
    ac-finalizer.cc \
    ac-finalizer.h \
    ac-forreplicate.cc \
    ac-forreplicate.h \
    ac-initializer.cc \
    ac-initializer.h \
    ac-peek.cc \
    ac-peek.h \
    ac-port.cc \
    ac-port.h \
    ac-portconnection.cc \
    ac-portconnection.h \
    ac-sharedcheck.cc \
    ac-sharedcheck.h \
    ac-sharedconnection.cc \
    ac-sharedconnection.h \
    ac-sharedupdate.cc \
    ac-sharedupdate.h \
    ac-state.cc \
    ac-state.h \
    ac-task.cc \
    ac-task.h \
    ac-taskgroup.cc \
    ac-taskgroup.h \
    ac-teamreplicate.cc \
    ac-teamreplicate.h \
    ac-userport.cc \
    ac-userport.h \
    ac-variable.cc \
    ac-variable.h \
    tl-acotescompilerphase.cc \
    tl-acotescompilerphase.h \
    tl-acotesinlinecompilerphase.cc \
    tl-acotesinlinecompilerphase.h \
    tl-acotesmaincompilerphase.cc \
    tl-acotesmaincompilerphase.h \
    tl-acoteslogger.cc \
    tl-acoteslogger.h \
    tl-acotesreplaceidexpression.cc \
    tl-acotesreplaceidexpression.h \
    tl-acotesstack.cc \
    tl-acotesstack.h \
    tl-acotestransform.cc \
    tl-acotestransform.h \
    tl-finalizerconstruct.cc \
    tl-finalizerconstruct.h \
    tl-finalizertransform.cc \
    tl-finalizertransform.h \
    tl-forreplicatetransform.cc \
    tl-forreplicatetransform.h \
    tl-initializerconstruct.cc \
    tl-initializerconstruct.h \
    tl-initializertransform.cc \
    tl-initializertransform.h \
    tl-peekconstruct.cc \
    tl-peekconstruct.h \
    tl-peektransform.cc \
    tl-peektransform.h \
    tl-portconnectiontransform.cc \
    tl-portconnectiontransform.h \
    tl-porttransform.cc \
    tl-porttransform.h \
    tl-sharedconstruct.cc \
    tl-sharedconstruct.h \
    tl-sharedtransform.cc \
    tl-sharedtransform.h \
    tl-statetransform.cc \
    tl-statetransform.h \
    tl-taskconstruct.cc \
    tl-taskconstruct.h \
    tl-taskgroupconstruct.cc \
    tl-taskgroupconstruct.h \
    tl-taskgrouptransform.cc \
    tl-taskgrouptransform.h \
    tl-tasktransform.cc \
    tl-tasktransform.h \
    tl-teamreplicateconstruct.cc \
    tl-teamreplicateconstruct.h \
    tl-teamreplicatetransform.cc \
    tl-teamreplicatetransform.h \
    tl-transform.cc \
    tl-transform.h \
    tl-userportconstruct.cc \
    tl-userportconstruct.h \
    tl-userporttransform.cc \
    tl-userporttransform.h \
    tl-variableclause.cc \
    tl-variableclause.h \
    tl-variabletransform.cc \
    tl-variabletransform.h

libtlacotes_output_la_CFLAGS = $(COMMON_CFLAGS)
libtlacotes_output_la_CXXFLAGS = $(COMMON_CFLAGS)

libtlacotes_output_la_LDFLAGS = $(tl_ldflags)
libtlacotes_output_la_LIBADD = $(tl_libadd)

libtlacotes_output_la_SOURCES = \
    acotes-output.cpp \
    acotes-output.hpp \
    acotes-outputtasks.cpp \
    acotes-outputtasks.hpp 

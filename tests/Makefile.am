AUTOMAKE_OPTIONS = dejagnu

RUNTESTFLAGS=--srcdir=$(srcdir) 

NULL=

ADDITIONAL_TESTS=@ADDITIONAL_TESTSUITES@

DEJATOOL = \
  mcxx.parser \
  mcc.typecalc \
  mcxx.typecalc \
  mcxx.torture \
  mcxx.cxx0x \
  mcc.extensions \
  mcxx.extensions \
  mcxx.gxx_type_traits \
  mcc.hlt \
  $(ADDITIONAL_TESTS) \
  $(NULL)


END=
CLEANFILES=

CONFIG_FILES_TESTS= \
                    mcxx.instr.nanos4.dg/config.mcxx \
                    mcxx.nanos4.dg/config.mcxx \
                    mcc.superscalar.dg/config.mcxx \
                    mcxx.stm.dg/config.mcxx \
                    mcc.instr.nanos4.dg/config.mcxx \
                    mcc.nanos4.dg/config.mcxx \
                    mcc.hlt.basic.dg/config.mcxx \
                    $(END)

CLEANFILES += $(CONFIG_FILES_TESTS)
BUILT_SOURCES=$(CONFIG_FILES_TESTS)

$(CONFIG_FILES_TESTS) : Makefile $(top_builddir)/config/replace.sh
	$(top_builddir)/config/replace.sh $(srcdir)/$@.in $@

dist-hook:
	-cp $(srcdir)/*.conf $(distdir)
	-for i in $(srcdir)/*.dg $(srcdir)/*.files; \
	do  \
	    DIR=$$(basename $$i); \
		mkdir -p $(distdir)/$${DIR} ; \
		cp $(srcdir)/$${DIR}/*.exp $(distdir)/$${DIR} ; \
		cp $(srcdir)/$${DIR}/config.mcxx.in $(distdir)/$${DIR} ; \
		cp $(srcdir)/$${DIR}/*.c $(distdir)/$${DIR} ; \
		cp $(srcdir)/$${DIR}/*.cpp $(distdir)/$${DIR} ; \
		cp $(srcdir)/$${DIR}/*.C $(distdir)/$${DIR} ; \
		cp $(srcdir)/$${DIR}/*.cc $(distdir)/$${DIR} ; \
	done

distclean-local:
	rm -vf lt-mcxx_success*.c
	rm -vf lt-mcxx_failure*.c
	rm -vf lt-mcxx_success*.cpp
	rm -vf lt-mcxx_failure*.cpp
	rm -vf *backtrace.txt
	rm -vf stm_unhandled_functions*.log

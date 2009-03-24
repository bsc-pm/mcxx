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
  mcxx.hlt \
  $(ADDITIONAL_TESTS) \
  $(NULL)


dist-hook:
	cp -vf $(srcdir)/*.conf $(distdir)
	for i in $(srcdir)/*.dg; \
	do  \
	    DIR=$$(basename $$i); \
		mkdir -p $(distdir)/$${DIR} ; \
		cp -v $(srcdir)/$${DIR}/*.exp $(distdir)/$${DIR} ; \
		cp -v $(srcdir)/$${DIR}/config.mcxx.in $(distdir)/$${DIR} ; \
		cp -v $(srcdir)/$${DIR}/*.c $(distdir)/$${DIR} ; \
		cp -v $(srcdir)/$${DIR}/*.cpp $(distdir)/$${DIR} ; \
	done

distclean-local:
	rm -vf lt-mcxx_success*.c
	rm -vf lt-mcxx_failure*.c
	rm -vf lt-mcxx_success*.cpp
	rm -vf lt-mcxx_failure*.cpp
	rm -vf stm_unhandled_functions*.log

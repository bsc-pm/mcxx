AUTOMAKE_OPTIONS = dejagnu

RUNTESTFLAGS=--srcdir=$(srcdir) 

NULL=

ADDITIONAL_TESTS=@ADDITIONAL_TESTSUITES@

DEJATOOL = \
  mcxx.parser \
  mcc.typecalc \
  mcxx.typecalc \
  mcxx.torture \
  $(ADDITIONAL_TESTS) \
  $(NULL)

dist-hook:
	for i in $(srcdir)/*.dg; \
	do  \
	    DIR=$$(basename $$i); \
		mkdir -p $(distdir)/$${DIR} ; \
		cp -v $(srcdir)/$${DIR}/*.exp $(distdir)/$${DIR} ; \
		cp -v $(srcdir)/$${DIR}/config.mcxx.in $(distdir)/$${DIR} ; \
		cp -v $(srcdir)/$${DIR}/*.c $(distdir)/$${DIR} ; \
		cp -v $(srcdir)/$${DIR}/*.cpp $(distdir)/$${DIR} ; \
	done

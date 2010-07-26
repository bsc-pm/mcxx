EXTRA_DIST=bets.in

CLEANFILES=
# These are a cache files used by generators
# that contain the potentially dlopened libraries
# so tests works even if the compiler is not installed
CLEANFILES+=omp-libraries-libtool
CLEANFILES+=ss-libraries-libtool
CLEANFILES+=stm-libraries-libtool

BUILT_SOURCES=bets

bets: $(srcdir)/bets.in
	cp $< $@
	chmod +x $@

CLEANFILES=bets

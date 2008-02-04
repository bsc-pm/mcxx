info_TEXINFOS=mcxx-internals.texi

doxygen : doxygen_tl
	@echo "-- Running doxygen"
	[ -e doxygen_tl ] || ( echo "File 'doxygen_tl' is missing"; exit 1; )
	doxygen doxygen_tl

doxygen-pdf : doxygen
	cd latex && make all

clean-local:
	-rm -fr html latex

TPP=$(top_builddir)/lib/tpp$(EXEEXT)
CLEANFILES=
END=

CONFIG_MCXX=\
            config.mcxx \
            $(END)

# We need a dist hook to keep these files in sync with their .in
# see below
CONFIG_FILES =  \
       config.acotes \
       config.acotes-msf \
       config.nanos_omp \
       config.plain \
       config.stm_nebelung \
       config.superscalar \
	   $(END)

dist-hook:
	for i in $(CONFIG_FILES) $(CONFIG_MCXX); \
    do \
       cp "$(srcdir)/$${i}.in" "$(distdir)" \
    done

EXTRA_DIST = replace.sh.in

CLEANFILES += $(CONFIG_FILES) $(CONFIG_MCXX) replace.sh

# Manual substitution ...
# -e 's|@VAR[@]|$(VAR)|g'
config_edit = \
     $(SED) \
     -e 's|@SED[@]|$(SED)|' \
     -e 's|@ACOLIB_INCLUDES[@]|$(ACOLIB_INCLUDES)|g' \
     -e 's|@ACOLIB_LIBS[@]|$(ACOLIB_LIBS)|g' \
     -e 's|@ACOTES_MSF_INCLUDES[@]|$(ACOTES_MSF_INCLUDES)|g' \
     -e 's|@ACOTES_MSF_LIBS[@]|$(ACOTES_MSF_LIBS)|g' \
     -e 's|@INSTRUMENTATION_PHASE[@]|$(INSTRUMENTATION_PHASE)|g' \
     -e 's|@MINTAKA_INCLUDES[@]|$(MINTAKA_INCLUDES)|g' \
     -e 's|@MINTAKA_LIBS[@]|$(MINTAKA_LIBS)|g' \
     -e 's|@NANOS_INCLUDES[@]|$(NANOS_INCLUDES)|g' \
     -e 's|@NANOS_LIBS[@]|$(NANOS_LIBS)|g' \
     -e 's|@NEBELUNG_INCLUDES[@]|$(NEBELUNG_INCLUDES)|g' \
     -e 's|@NEBELUNG_LIBS[@]|$(NEBELUNG_LIBS)|g' \
     -e "s|@SUPERSCALAR_INCLUDES[@]|$(SUPERSCALAR_INCLUDES)|g" \
     -e "s|@SUPERSCALAR_LIBS[@]|$(SUPERSCALAR_LIBS)|g" \
     -e "s|@abs_top_srcdir[@]|$(abs_top_srcdir)|g" \
     -e 's|@prefix[@]|$(prefix)|g' \
     -e 's|@includedir[@]|$(includedir)|g' \
     -e 's|@libdir[@]|$(libdir)|g' \
     -e 's|@superscalar_version[@]|$(superscalar_version)|g' \
    $(END)

# A dependence with the Makefile?
replace.sh : Makefile
	rm -f $@ $@.tmp
	$(config_edit) '$(srcdir)/$@.in' > $@.tmp
	chmod +x $@.tmp
	chmod a-w $@.tmp
	mv $@.tmp $@

replace.sh : replace.sh.in

$(CONFIG_MCXX) $(CONFIG_FILES) : Makefile replace.sh
	./replace.sh "$(srcdir)/$@.in" "$@"

dist_pkgdata_DATA = \
		config.mcxx \
		$(END)

configdir = $(pkgdatadir)/config.d
dist_config_DATA = \
        $(CONFIG_FILES)

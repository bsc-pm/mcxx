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
       config.omp \
       config.plain \
       config.stm_nebelung \
       config.superscalar \
	   $(END)

INSTALL_CONFIG_FILES = @INSTALL_CONFIG_FILES@

dist-hook:
	for i in $(CONFIG_FILES) $(CONFIG_MCXX); \
    do \
       cp "$(srcdir)/$${i}.in" "$(distdir)" ; \
    done

CLEANFILES += $(CONFIG_FILES) $(CONFIG_MCXX)

# Replacements are defined in this file
include ./Makefile.replace

# This is valid in parallel make since we are generating one goal file at a time
$(CONFIG_MCXX) $(CONFIG_FILES) : Makefile
	$(config_edit) "$(srcdir)/$@.in" > "$@.tmp"
	mv -f "$@.tmp" "$@"

pkgdata_DATA = \
		config.mcxx \
		$(END)

configdir = $(pkgdatadir)/config.d
config_DATA = \
        $(INSTALL_CONFIG_FILES)

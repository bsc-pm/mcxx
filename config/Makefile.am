TPP=$(top_builddir)/lib/tpp
CLEANFILES=

CONFIG_MCXX_IN = config.mcxx.in

CONFIG_FILES_IN =  \
       config.acotes.in \
       config.nanos_omp.in \
       config.plain.in \
       config.stm_nebelung.in \
       config.superscalar.in \
	   $(END)

EXTRA_DIST = \
	   $(CONFIG_MCXX_IN) \
	   $(CONFIG_FILES_IN) \
	   $(END)

CONFIG_FILES=$(patsubst %.in,%,$(CONFIG_FILES_IN))

dist_pkgdata_DATA = \
		config.mcxx \
		$(END)

DEFINES=$(addprefix -D,@ENABLED_PROFILES@)

CLEANFILES+=config.mcxx

config.mcxx : $(TPP) $(CONFIG_FILES)
	$(TPP) -o $@ -I $(srcdir) $(DEFINES) $(srcdir)/config.mcxx.in

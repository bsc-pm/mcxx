TPP=$(top_builddir)/lib/tpp$(EXEEXT)
CLEANFILES=

CONFIG_MCXX_IN = config.mcxx.in

CONFIG_FILES_IN =  \
       config.acotes.in \
       config.acotes-msf.in \
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

configdir = $(pkgdatadir)/config.d
dist_config_DATA = \
        $(CONFIG_FILES)

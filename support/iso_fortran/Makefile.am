CLEANFILES =

configdir=$(pkgdatadir)/fortran
config_DATA = \
			  iso_fortran_env.mf03

PLAINFC=$(top_builddir)/src/driver/mcxx --config-dir=$(top_builddir)/config --profile=plainfc --do-not-warn-config

if SUPPORTED_SILENT_RULES
PLAINFC_verbose = $(PLAINFC_verbose_$(V))
PLAINFC_verbose_ = $(PLAINFC_verbose_$(AM_DEFAULT_VERBOSITY))
PLAINFC_verbose_0 = @echo "  PLAINFC" $@;
endif

iso_fortran_env.mf03 : $(srcdir)/iso_fortran_env.f90
	$(PLAINFC_verbose)$(PLAINFC) -y -o /dev/null $(srcdir)/iso_fortran_env.f90

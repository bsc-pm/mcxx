noinst_PROGRAMS=iso_fortran_env_generator

iso_fortran_env_generator_SOURCES=\
	 iso_fortran_env_generator.f90

CLEANFILES =

configdir=$(pkgdatadir)/fortran
config_DATA = \
			  iso_fortran_env.mf03

PLAINFC=$(top_builddir)/src/driver/mcxx --config-dir=$(top_builddir)/config --profile=plainfc

if SUPPORTED_SILENT_RULES
PLAINFC_verbose = $(PLAINFC_verbose_$(V))
PLAINFC_verbose_ = $(PLAINFC_verbose_$(AM_DEFAULT_VERBOSITY))
PLAINFC_verbose_0 = @echo "  PLAINFC" $@;
endif

iso_fortran_env.mf03 : iso_fortran_env.f90
	$(PLAINFC_verbose)$(PLAINFC) -y -o /dev/null iso_fortran_env.f90

iso_fortran_env.f90 : iso_fortran_env_generator
	$(AM_V_GEN)( ./iso_fortran_env_generator > $@ )

CLEANFILES += iso_fortran_env.f90

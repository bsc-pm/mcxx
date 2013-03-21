tl_cflags= -Wall -Wshadow \
    -I $(top_srcdir)/src/tl/codegen/common \
    -I $(top_srcdir)/src/tl/codegen/vector \
    -I $(top_srcdir)/src/tl/codegen/base/cxx \
    -I $(top_srcdir)/src/tl/codegen/base/fortran \
    -I $(top_srcdir)/src/tl/codegen/gpu/cuda \
    -I $(top_srcdir)/src/tl \
    -I $(top_srcdir)/src/mcxx_tl \
    -I $(top_srcdir)/src/frontend \
    -I $(top_srcdir)/src/frontend/fortran \
    -I $(top_srcdir)/src/driver \
    -I $(top_srcdir)/lib \
    -I $(top_builddir)/src/tl/codegen/common \
    -I $(top_builddir)/src/tl/codegen/vector \
    -I $(top_builddir)/src/tl/codegen/base/cxx \
    -I $(top_builddir)/src/tl/codegen/base/fortran \
    -I $(top_builddir)/src/tl/codegen/gpu/cuda \
    -I $(top_builddir)/src/tl \
    -I $(top_builddir)/src/mcxx_tl \
    -I $(top_builddir)/src/frontend \
    -I $(top_builddir)/src/frontend/fortran \
    -I $(top_builddir)/src/driver \
    -I $(top_builddir)/lib \
	$(END)

tl_cxxflags=$(tl_cflags)

tl_libadd=$(top_builddir)/src/tl/libtl.la \
        $(top_builddir)/src/frontend/libmcxx-process.la \
        $(top_builddir)/src/frontend/libmcxx.la \
		$(END)

tl_no_undefined=@WIN32_NO_UNDEFINED@

tl_ldflags=-avoid-version $(tl_no_undefined)
if WINDOWS_BUILD
else
tl_ldflags += \
    -Wl,@RPATH_PARAMETER@,\$$ORIGIN \
    -Wl,@RPATH_PARAMETER@,\$$ORIGIN/mcxx \
	-Wl,-z,origin \
	--enable-new-dtags \
	$(END)
endif

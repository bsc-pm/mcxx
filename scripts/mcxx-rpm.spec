# Good tutorials:
# http://freshrpms.net/docs/fight/
# Repos:
# http://eureka.ykyuen.info/2010/01/06/opensuse-create-your-own-software-repository-1/
# http://en.opensuse.org/SDB:Creating_YaST_installation_sources

%define name		mcxx
%define release		6
%define buildroot       %{_topdir}/%{name}-%{version}-root
%define nanox_dir       %{_prefix}
%define nanox_lib       %{_libdir}
%define mcxx_libs       find %{1} -regextype posix-egrep -not -regex "([^/]*/)+[^/]+(omp|nano|ss|superscalar).*"
# Forget about the .la files for now
%define _unpackaged_files_terminate_build 0

# Override prefix if _rpm_prefix is given
%{?_rpm_prefix: %define _prefix  %{_rpm_prefix} }

BuildRoot:	        %{buildroot}
Summary: 		The Mercurium source to source compiler
License: 		GPL
Name: 			%{name}
Version: 		%{version}
Release: 		%{release}
Source: 		%{name}-%{version}.tar.gz
Prefix: 		%{_prefix}
Group: 			Development/Tools
BuildRequires:		bison, flex, sqlite3-devel >= 3.6.16, gperf
Requires:		sqlite3 >= 3.6.16, gcc-fortran, binutils

%description
The Mercurium source to source compiler.

%prep
%setup -q

%build
# NOTE (gmiranda): we might need to pass --with-nanox-lib
%configure --enable-ompss --with-nanox=%{nanox_dir} --with-nanox-lib=%{nanox_lib}
make -j4

#%check
#make check

%install
%makeinstall
find %{buildroot}%{_libdir} -regextype posix-egrep -not -regex "([^/]*/)+[^/]+(omp|nano|ss|superscalar).*" | grep "\.so" > tmp.list
sed -e "s|^%{buildroot}||" < tmp.list > mcxx_files.list
find %{buildroot}%{_libdir} -regextype posix-egrep -regex "([^/]*/)+[^/]+(omp|nano|ss|superscalar).*" | grep "\.so" > tmp.list
sed -e "s|^%{buildroot}||" < tmp.list > ompss_files.list
rm tmp.list
#%{_topdir}/fix-paths.sh %{nanox_dir} %{prefix} %{nanox_lib} %{prefix}/lib64 %{buildroot}/%{_datadir}/mcxx/config.d/

%files
#%files -f mcxx_files.list
%defattr(-,root,root)
%{_bindir}/*
%{_libdir}/libanalysis*.so
%{_libdir}/libauto_scope.so
%{_libdir}/libcodegen*.so
%{_libdir}/libliveness.so
%{_libdir}/libloops_analysis.so
%{_libdir}/libmcxx*.so
%{_libdir}/libmf03*.so
%{_libdir}/libpcfg.so
%{_libdir}/libreaching_definitions.so
%{_libdir}/libtl*.so
%{_libdir}/libuse_def.so
%{_libdir}/mcxx/libcodegen*.so
%{_libdir}/mcxx/libtest_analysis.so
%{_libdir}/mcxx/libtl-hlt*.so
%{_libdir}/mcxx/libtlvector*.so

#%{mcxx_libs}
%{_datadir}/mcxx/config.mcxx
%{_datadir}/mcxx/analysisdata/cLibraryFunctionList
%{_datadir}/mcxx/fortran/*
%{_datadir}/mcxx/config.d/00.config.plain
%{_datadir}/mcxx/config.d/10.config.analysis-base

#%{_datadir}/mcxx/config.d/*

#%doc %attr(0444,root,root) /usr/local/share/man/man1/wget.1


%package ompss
Requires:               nanox, mcxx
BuildRequires:          nanox
Summary: 		Mercurium OmpSs support
Group: 			Development/Tools
Provides: 		ompss

%description ompss
OmpSs support for the Mercurium source to source compiler.

#%files ompss -f ompss_files.list
%files ompss
%defattr(-,root,root)
%{_libdir}/mcxx/libtlomp*.so
%{_libdir}/mcxx/libtlnanos-version.so
%{_libdir}/mcxx/libtlnanox-*.so

%{_datadir}/mcxx/config.d/10.config.omp-base
%{_datadir}/mcxx/config.d/50.config.cuda
%{_datadir}/mcxx/config.d/50.config.gpu
%{_datadir}/mcxx/config.d/50.config.omp.*
%{_datadir}/mcxx/config.d/57.config.omp.smp

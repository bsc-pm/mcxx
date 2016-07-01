# Good tutorials:
# http://freshrpms.net/docs/fight/
# Repos:
# http://eureka.ykyuen.info/2010/01/06/opensuse-create-your-own-software-repository-1/
# http://en.opensuse.org/SDB:Creating_YaST_installation_sources

%if 0%{?suse_version}
%define distro          opensuse%{?suse_version}
%else
%define distro          %{?dist}
%endif
%define _name            mcxx
%define buildroot       %{_topdir}/%{_name}-%{_version}-root
%define nanox_dir       %{_prefix}
%define nanox_lib       %{_libdir}
%define debug_package   %{nil}
# Forget about the .la files for now
%define _unpackaged_files_terminate_build 0

# Override prefix if _rpm_prefix is given
%{?_rpm_prefix: %define _prefix  %{_rpm_prefix} }

BuildRoot:     %{buildroot}
Summary:       The Mercurium source to source compiler
License:       LGPLv2
Name:          %{_name}
Version:       %{_version}
Release:       %{_release}%{distro}
Source:        %{_name}-%{_version}.tar.gz
Prefix:        %{_prefix}
Group:         Development/Tools
Provides:      ompss
%if 0%{?suse_version}
BuildRequires: bison, flex, sqlite3-devel >= 3.6.16, gperf, nanox, texinfo, pkg-config
Requires:      sqlite3 >= 3.6.16, gcc-fortran, binutils, nanox
%else
BuildRequires: bison, flex, sqlite-devel >= 3.6.16, gperf, nanox, texinfo, pkgconfig
Requires:      sqlite >= 3.6.16, gcc-gfortran, binutils, nanox
%endif
%description
The Mercurium source to source compiler, with OmpSs support.

%prep
%setup -q

%build
%configure --enable-ompss --with-nanox=%{nanox_dir} --with-nanox-lib=%{nanox_lib}
make -j%{threads}

#%check
#make check

%install
make install DESTDIR=%{buildroot}

%files
%defattr(-,root,root)
%{_bindir}/*
%{_libdir}/*
%{_libdir}/mcxx/*
# %{_datadir}/mcxx/intel-omp/
%{_datadir}/mcxx/*
%{_datadir}/mcxx/analysisdata/*
%{_datadir}/mcxx/config.d/*
%{_datadir}/mcxx/fortran/*
%{_datadir}/mcxx/nanos/*
%{_datadir}/mcxx/romol/*

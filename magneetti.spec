%define pypi_name magneetti

Name:         magneetti
Version:      0.1
Release:      alt1

Summary:      Calculate cylindrically-symmetric magnets
Group:        System
URL:          https://github.com/slazav/magneetti
License:      GPL

Packager:     Vladislav Zavjalov <slazav@altlinux.org>

Source:       %name-%version.tar

BuildRequires(pre): rpm-build-python3
BuildRequires: python3-devel

%description
magneetti - calculate cylindrically-symmetric magnets.

%prep
%setup -q

%build
%make

%install
%makeinstall

%files
%_bindir/magnet*
%dir %python3_sitelibdir/magnet
%python3_sitelibdir/magnet/*

%changelog

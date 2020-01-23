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
mkdir -p %buildroot/%python3_sitelibdir/magnet
sed 's|../magnet_new|magnet_new|' test_py/magnet.py  > %buildroot/%python3_sitelibdir/magnet/magnet.py
cp test_py/__init__.py %buildroot/%python3_sitelibdir/magnet/


%files
%_bindir/magnet_new
%python3_sitelibdir/magnet/*

%changelog

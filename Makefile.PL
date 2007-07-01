use 5.008;

my($inc,$libs,$cflags) = ('','','');

my %missing_modules;

$missing_modules{chk_module('POSIX')} = 1;
$missing_modules{chk_module('Carp')} = 1;
$missing_modules{chk_module('File::Basename')} = 1;
$missing_modules{chk_module('Geo::GDAL')} = 1;
$missing_modules{chk_module('ExtUtils::MakeMaker')} = 1;
$missing_modules{chk_module('ExtUtils::Depends')} = 1;
$missing_modules{chk_module('Geo::Raster')} = 1;
$missing_modules{chk_module('Geo::Distance')} = 1;
$missing_modules{chk_module('Graph')} = 1;
delete $missing_modules{''};

my @missing_modules = keys %missing_modules;
warn "Following required modules are missing: @missing_modules\n" if @missing_modules;

use ExtUtils::MakeMaker;
use ExtUtils::Depends; 
use ExtUtils::PkgConfig;

my($inc,$libs,$cflags) = ('','','');

for $package ('gdal','gdk-pixbuf-2.0','libral') {
    print "Checking for $package: ";
    my($c,$l);
    if ($package eq 'gdal' and $^O ne 'MSWin32') {
	$c = `gdal-config --cflags`;
	chomp $c;
	$l = `gdal-config --libs`;
	chomp $l;
    } else {
	%pkg_info = ExtUtils::PkgConfig->find ($package);
	$c = $pkg_info{cflags};
	$l = $pkg_info{libs};
    }
    $cflags .= " $c";
    # this code is for MinGW in fact
    if ($^O eq "MSWin32") {
	my @a = split /\s+/,$l;
	for (@a) {
	    if (/^-l/ and !(/.dll/)) {
		$_ .= '.dll.a';
	    }
	}
	$l = join(' ',@a);
    }
    $libs .= " $l";
    print "ok\n";
}

$package = new ExtUtils::Depends('Geo::Vector');

$package->set_inc($inc);
$package->set_libs($libs);

$package->add_typemaps("typemap");

$package->save_config("Files.pm");

our %pm_files = (
	'lib/Geo/Vector.pm'          => '$(INST_LIBDIR)/Vector.pm',
);

$package->add_pm (%pm_files);

#       

WriteMakefile(
	      NAME              => 'Geo::Vector',
	      VERSION_FROM      => 'lib/Geo/Vector.pm',
	      $package->get_makefile_vars(),
	      ( $] >= 5.005 ?
		(ABSTRACT_FROM  => 'lib/Geo/Vector.pm',
		 AUTHOR         => 'Ari Jolma <ajolma at tkk.fi>') : () ),
	      CCFLAGS => "-O2 -fno-builtin $cflags $inc",
	      );
if  (eval {require ExtUtils::Constant; 1}) {
  # If you edit these definitions to change the constants used by this module,
  # you will need to use the generated const-c.inc and const-xs.inc
  # files to replace their "fallback" counterparts before distributing your
  # changes.
  my @names = (qw(CONCAVE CONVEX));
  ExtUtils::Constant::WriteConstants(
                                     NAME         => 'Geo::Vector',
                                     NAMES        => \@names,
                                     DEFAULT_TYPE => 'IV',
                                     C_FILE       => 'const-c.inc',
                                     XS_FILE      => 'const-xs.inc',
                                  );

}
else {
  use File::Copy;
  use File::Spec;
  foreach my $file ('const-c.inc', 'const-xs.inc') {
    my $fallback = File::Spec->catfile('fallback', $file);
    copy ($fallback, $file) or die "Can't copy $fallback to $file: $!";
  }
}

sub chk_module {
    my $pkg = shift;
    print "Checking for $pkg:";
    eval { 
	my $p; 
	($p = $pkg . ".pm") =~ s#::#/#g; 
	require $p; 
    };
    if ($@) {
	print " failed\n";
	return $pkg;
    } else {
	print " ok\n";
	return '';
    }
}

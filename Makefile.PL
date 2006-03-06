use 5.008;
use ExtUtils::MakeMaker;
use ExtUtils::Depends; 

chk_module('gdal','ogr','Geo::Raster');
if (%missing_modules) {
    print "Following required modules are missing:";
    for (keys %missing_modules) {
	print " $_";
    }
    print "\n";
    exit;
}

# search for ral_config.h

my($pgplot_inc,$pgplot_libs,$netpbm_libs,$ral_libs) = ('','','','');

my $ral_config;
for ('/usr/include','/usr/local/include',$ENV{RAL_DIR},'../libral') {
    print "testing: $_/ral_config.h\n";
    next unless -f "$_/ral_config.h";
    $ral_dir = $_;
    $ral_config = "$_/ral_config.h";
    last;
}
unless ($ral_config) {
    print "Can't find ral_config.h, unable to continue.\n";
    print "Consider export RAL_DIR=DIR; perl Makefile.PL\n";
    exit 1;
}
print "Using $ral_config for libral configuration.\n";

$ral_inc = " -I$ral_dir";
if ($ral_dir =~ /\/include$/) {
    $ral_dir =~ s/\/include$//;
    $ral_dir .= '/lib';
} else {
    $ral_dir .= '/.libs';
}
$ral_libs = " -L$ral_dir -lral";

$ral_libs .= " -lgdal";


# check for the modules required by Geo::Raster

sub chk_module {
    for my $pkg (@_) {
	print "Checking for $pkg ..";
	eval { 
	    my $p; 
	    ($p = $pkg . ".pm") =~ s#::#/#g; 
	    require $p; 
	};
	if ($@) {
	    print " failed\n";
	    $missing_modules{$pkg} = 1;
	} else {
	    print " ok\n";
	}
    }
}

my $GLIB_CFLAGS = `pkg-config --cflags glib-2.0`;
chomp $GLIB_CFLAGS;
my $GLIB_LIBS = `pkg-config --libs glib-2.0`;
chomp $GLIB_LIBS;

my $GTK_CFLAGS = `pkg-config --cflags gtk+-2.0`;
chomp $GTK_CFLAGS;
my $GTK_LIBS = `pkg-config --libs gtk+-2.0`;
chomp $GTK_LIBS;

$package = new ExtUtils::Depends('Geo::Vector');

$package->set_inc($ral_inc);
$package->set_libs($ral_libs .' ' . $GLIB_LIBS .' ' . $GTK_LIBS);

$package->add_typemaps("typemap");

$package->save_config("Files.pm");

our %pm_files = (
	'lib/Geo/Vector.pm'          => '$(INST_LIBDIR)/Vector.pm',
);

$package->add_pm (%pm_files);

#       

WriteMakefile(
	      NAME              => 'Geo::Vector',
	      VERSION_FROM      => 'lib/Geo/Vector.pm', # finds $VERSION
	      $package->get_makefile_vars(),
	      PREREQ_PM         => {}, # e.g., Module::Name => 1.1
	      ($] >= 5.005 ?     ## Add these new keywords supported since 5.005
	       (ABSTRACT_FROM  => 'lib/Geo/Vector.pm', # retrieve abstract from module
		AUTHOR         => 'Ari Jolma <ajolma at tkk.fi>') : ()),
#	      LIBS              => ['-lgdal -lral'], # e.g., '-lm'
#	      DEFINE            => '', # e.g., '-DHAVE_SOMETHING'
#	      INC               => '-I.', # e.g., '-I. -I/usr/include/other'
	      CCFLAGS => '-O2 -fno-builtin -ansi '.$GLIB_CFLAGS.' '.$GTK_CFLAGS
	      # Un-comment this if you add C files to link with later:
	      # OBJECT            => '$(O_FILES)', # link all the C files too
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

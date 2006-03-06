# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl Geo-Vector.t'

#########################

# change 'tests => 2' to 'tests => last_test_to_print';

use Test::More tests => 2;
BEGIN { use_ok('Geo::Vector') };

my $fail = 0;
foreach my $constname (qw(
	CONCAVE CONVEX)) {
  next if (eval "my \$a = $constname; 1");
  if ($@ =~ /^Your vendor has not defined Geo::Vector macro $constname/) {
    print "# pass: $@";
  } else {
    print "# fail: $@";
    $fail = 1;
  }

}

ok( $fail == 0 , 'Constants' );
#########################

# Insert your test code below, the Test::More module is use()ed here so read
# its man page ( perldoc Test::More ) for help writing this test script.

use Carp;

#$world = new Geo::Vector "/home/gis/maapera/1143/m114301/ARC";


#$lc = new Geo::Raster filename=>'/home/ajolma/proj/clime/climate_data/lc_clip.bil',load=>1;
#$bg = $world->rasterize(like=>$lc);
#$grid = new Geo::Vector '/home/ajolma/proj/clime/climate_data/RCAO_l.shp';
#$raster = $grid->rasterize(like=>$lc,value_from=>'rlat_rlon');

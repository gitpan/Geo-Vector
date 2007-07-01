# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl Geo-Vector.t'

#########################

use Test::More qw(no_plan);
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

#goto here;

for ('dbf','prj','shp','shx') {
    unlink "t/test.$_";
}

eval {
    $test = new Geo::Vector(datasource=>'foo');
};
ok($@ eq '', 'open anything as a datasource');

@layers = (1);
if ($test) {
    $layers = $test->layers;
    @layers = keys %$layers;
}
ok(@layers == 0, 'blank datasource does not have any layers');

eval {
    $test = new Geo::Vector(driver=>'ESRI Shapefile',datasource=>'./t');
};
ok($@ eq '', 'create a new dataset');

for (keys %Geo::Vector::GEOMETRY_TYPE) {
    next if $_ =~ /GeometryC/;
    next if $_ =~ /Multi/;
    next if $_ =~ /Unkno/;
    eval {
	$test->layer(layer=>'test'.$_, geometry_type=>$_);
    };
    print STDERR "$@\n" if $@;
    ok($_ eq $test->geometry_type(),"Create layer with $_ type");
    #$test->delete_layer('test'.$_);
    for my $e ('dbf','prj','shp','shx') {
	unlink "t/test$_.$e";
    }
}

eval {
    $test->layer(layer=>'test', geometry_type=>'Point');
};
print STDERR "$@\n" if $@;
ok($@ eq '', 'create a layer into the new dataset');

eval {
    $test->schema({int=>{Number=>0, TypeName=>'Integer'}, real=>{Number=>1, TypeName=>'Real'}});
};
print STDERR "$@\n" if $@;
ok($@ eq '', 'add a schema into the layer');

eval {
    $test->add_feature(geometry=>[1.123,2.345],int=>12,real=>3.4);
    $test->add_feature(geometry=>[2.123,2.345],int=>13,real=>4.4);
    $test->add_feature(geometry=>[0.123,2.345],int=>15,real=>7.4);
};
print STDERR "$@\n" if $@;
ok($@ eq '', 'add a feature into the layer');

@range = $test->value_range(field_name=>'int',filter_rect=>[1,2,3,3]);

ok($range[1] == 13, 'value_range with filter_rect');
undef $test;

eval {
    $test = new Geo::Vector(datasource=>'./t', layer=>'test');
};
print STDERR "$@\n" if $@;
ok($@ eq '', 'open a layer');

ok ($test->feature_count == 3, 'feature_count');
ok ($test->field_count == 2, 'field_count');

@w = $test->world;
ok (@w == 4, 'world size');

eval {
    $f = $test->feature(0);
};

print STDERR "$@\n" if $@;
ok($@ eq '', 'retrieve a feature');

ok (abs($f->{geometry}->[0]-1.123) < 0.01, 'returns correct data');
ok (abs($f->{real}-3.4) < 0.01, 'returns correct attr');

for ('dbf','prj','shp','shx') {
    unlink "t/test.$_";
}

# test feature set
here:
    print STDERR "\n";
$v = Geo::Vector->new();

$v->add_feature(sfield => 'string', ifield => 1, rfield => 1.23, geometry => [12.34, 56.78], geometry_type => 'Point');

ok($v->feature_count == 1, 'fset: feature count');

my $c = $v->field_count(feature=>0);

ok($c == 3, 'fset: field count');

$s = $v->schema(0);

ok($s->{rfield}{TypeName} eq 'Real', 'fset: schema');
ok($s->{sfield}{TypeName} eq 'String', 'fset: schema');

#$v->render();

package Geo::Vector;

use 5.008;
use strict;
use warnings;
use Carp;
use File::Basename;
use gdal;
use ogr;
use Geo::Raster;

require Exporter;
use AutoLoader;

our @ISA = qw(Exporter);

use vars qw(@ogr_geom_types %ogr_geom_types %COLOR_SCHEMES);

# Items to export into callers namespace by default. Note: do not export
# names by default without a very good reason. Use EXPORT_OK instead.
# Do not simply export all your public functions/methods/constants.

# This allows declaration	use Geo::Vector ':all';
# If you do not need this, moving things directly into @EXPORT or @EXPORT_OK
# will save memory.
our %EXPORT_TAGS = ( 'all' => [ qw(
	@ogr_geom_types %ogr_geom_types %COLOR_SCHEMES
) ] );

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

@ogr_geom_types = ('wkbUnknown','wkbPoint','wkbLineString','wkbPolygon',
		   'wkbMultiPoint','wkbMultiLineString','wkbMultiPolygon',
		   'wkbGeometryCollection',
		   'wkbPoint25D','wkbLineString25D','wkbPolygon25D',
		   'wkbMultiPoint25D','wkbMultiLineString25D','wkbMultiPolygon25D',
		   'wkbGeometryCollection25D');

%ogr_geom_types = ();

for (@ogr_geom_types) {
    my $v = eval "\$ogr::$_";
    $ogr_geom_types{$v} = $_;
    $ogr_geom_types{$v} =~ s/^wkb//;
};

%COLOR_SCHEMES = (Grayscale => 0, Rainbow => 1, Colortable => 2, 'Single color' => 3);

our $VERSION = '0.42';

sub AUTOLOAD {
    # This AUTOLOAD is used to 'autoload' constants from the constant()
    # XS function.

    my $constname;
    our $AUTOLOAD;
    ($constname = $AUTOLOAD) =~ s/.*:://;
    croak "&Geo::Vector::constant not defined" if $constname eq 'constant';
    my ($error, $val) = constant($constname);
    if ($error) { croak $error; }
    {
	no strict 'refs';
	# Fixed between 5.005_53 and 5.005_61
#XXX	if ($] >= 5.00561) {
#XXX	    *$AUTOLOAD = sub () { $val };
#XXX	}
#XXX	else {
	    *$AUTOLOAD = sub { $val };
#XXX	}
    }
    goto &$AUTOLOAD;
}

require XSLoader;
XSLoader::load('Geo::Vector', $VERSION);

=pod

=head1 NAME

Geo::Vector - Perl extension for geospatial vectors

=head1 SYNOPSIS

    use Geo::Vector;

    use Geo::Vector qw(:all);

=head1 EXPORT

None by default. :all imports @ogr_geom_types, %ogr_geom_types, and
%COLOR_SCHEMES.

@ogr_geom_types is a list of ogr type names:
'wkbUnknown','wkbPoint',...

keys of %ogr_geom_types are the values $ogr::wkbUnknown, ... and the
values are the type names.

%COLOR_SCHEMES = (Grayscale => 0, Rainbow => 1, Colortable => 2, 'Single color' => 3);

=head1 DESCRIPTION

Geo::Vector is an extra layer on top of Perl module ogr. ogr module
allows using OGR (from GDAL) from Perl. Geo::Vector adds the required
functionality to use ogr layers in Gtk2::Ex::Geo. Geo::Vector may also
add extra functionality like rasterization, vectorization, rendering
etc. which may be useful in stand-alone scripts.

=head1 BASIC FUNCTIONALITY

=head2 Constructors

Simply open a geospatial vector file:

    $v = new Geo::Vector("borders.shp");

or use named parameters:

    $v = new Geo::Vector(filename=>"borders.shp");

    $v = new Geo::Vector(datasource=>".", layer=>"borders.shp");

Named parameters:

=over

=item driver 

Default is ''. Used only when creating a new layer. If not false,
ogr_datasource is created using the specified driver and
ogr::Driver::CreateDataSource. This parameter may be an ogr::Driver
object, number, or driver name.

=item driver_options

Default is []. Given directly to ogr::CreateDataSource.

=item filename

Default is ''. Parsed to datasource and layer if given.

=item datasource 

Default is ''. Datasource string as used in
ogr::Driver::CreateDataSource and ogr::Open.

=item layer

Default is ''. Layer string as used in ogr::Driver::CreateDataSource
and ogr::Open.

=item update

Default is ''. True/False as used in ogr::Open.

=item srs

Default is ''. TODO.

=item geom_type

Default is $ogr::wkbUnknown. Used in ogr::Datasource::CreateLayer.

=item sql

Default is ''. If not null string, layer is obtained with
ogr::Datasource::ExecuteSQL and ReleaseResultSet is called
automatically in destruction.

=back

=head2 Drivers

    %d = Geo::Vector::drivers();

Keys of %d are numbers. Retrieves values by
ogr::GetDriver. $d{<number>}->{name} is the name of the driver.

=cut

sub drivers {
    my %d;
    for (0..ogr::GetDriverCount()-1) {
	my $d = ogr::GetDriver($_);
	$d{$_} = $d;
    }
    return %d;
}

# currently assuming only one layer
sub new {
    my $package = shift;
    my %options;
    if (@_ == 1) {
	$options{filename} = $_[0];
    } else {
	%options = @_;
    }
    my $self = {};

    croak "usage: new Geo::Vector (datasource=>'...',layer=>'...',...);" 
	unless $options{filename} or ($options{datasource} and $options{layer});

    my %defaults = ( driver => '',
		     driver_options => [],
		     filename => '',
		     datasource => '',
		     layer => '',
		     update => 0,
		     srs => 0,
		     geom_type => $ogr::wkbUnknown,
		     sql => '',
		     );

    for (keys %defaults) {
	next if defined $self->{$_};
	$self->{$_} = $defaults{$_};
    }
    
    for (keys %options) {
	croak "unknown constructor option for Geo::Vector: $_" 
	    unless defined $defaults{$_}
    }
    for (keys %defaults) {
	next unless defined $options{$_};
	$self->{$_} = $options{$_};
    }

    $self->{update} = 1 if $self->{driver};

    ($self->{layer},$self->{datasource},undef) = fileparse($self->{filename}) if $self->{filename};

    # open the datasource

    if (ref($self->{driver}) eq 'ogr::Driver') {
	$self->{ogr_driver} = $self->{driver};
    } elsif ($self->{driver} =~ /^\d+$/) {
	$self->{ogr_driver} = ogr::GetDriver($self->{driver});
    } elsif ($self->{driver}) {
	$self->{ogr_driver} = ogr::GetDriverByName($self->{driver});
    }

    if ($self->{ogr_driver}) {
	$self->{ogr_datasource} = $self->{ogr_driver}->
	    CreateDataSource($self->{datasource},$self->{driver_options}) or
	    croak "could not create datasource $self->datasource}";
    } else {
	$self->{ogr_datasource} = ogr::Open($self->{datasource},$self->{update}) or
	    croak "ogr::Open $self->{datasource} failed";
    }

    $self->{OGRDataSourceH} = get_OGRDataSourceH($self->{ogr_datasource});

    # SRS may be global, and then it is given as input
    # SRS may be from the layer, then it is available from the layer
    #    if SRS from layer is different from global SRS, then we reproject...
    #    TODO: add reprojecting to rasters....
    # if all else fails we say this is WGS84
    unless ($self->{srs}) {
	$self->{srs} = new osr::SpatialReference;
	$self->{srs}->SetWellKnownGeogCS('WGS84');
    }

    # get or create layer
    unless ($self->{driver}) {
#	$self->{layer} =~ s/\.\w+$//;
#	print STDERR "\nget by name $self->{layer}\n";
	my @layers;
	for (0..$self->{ogr_datasource}->GetLayerCount-1) {
	    my $l = $self->{ogr_datasource}->GetLayerByIndex($_);
	    push @layers,$l->GetName;
	}

	if ($self->{sql}) {

	    $self->{ogr_layer} = $self->{ogr_datasource}->ExecuteSQL($self->{sql});

	} else {

	    $self->{ogr_layer} = $self->{ogr_datasource}->GetLayerByName($self->{layer});

	    croak "could not get layer by name $self->{layer}, available layers are: @layers" unless $self->{ogr_layer};
	}
    } else {

	print STDERR "\ncreate $self->{layer}\n";

	$self->{ogr_layer} = $self->{ogr_datasource}->CreateLayer($self->{layer},$self->{srs},$self->{geom_type});
	croak "could not create layer $self->{layer}" unless $self->{ogr_layer};
    }

    
    
    $self->{OGRLayerH} = get_OGRLayerH($self->{ogr_layer});

    $self->{type} = ref($self);

    bless $self => (ref($package) or $package);
    
    return $self;
}

sub DESTROY {
    my $self = shift;
    return unless $self;
    if ($self->{sql}) {
	$self->{ogr_datasource}->ReleaseResultSet($self->{ogr_layer}) if $self->{ogr_datasource};
    }
}

=pod

=head2 Feature count

    $feature_count = $vector->feature_count;

=cut

sub feature_count {
    my($self) = @_;
    return unless $self->{ogr};
    return $self->{ogr_layer}->GetFeatureCount();
}

=pod

=head2 Get/set color scheme

    $color_scheme = $vector->color_scheme($color_scheme);

Sets the color_scheme if $color_scheme is defined. Color schemes are
keys of %Geo::Vector::COLOR_SCHEMES. The returned value is a value
from %Geo::Vector::COLOR_SCHEMES.

=cut

sub color_scheme {
    my($self,$color_scheme) = @_;
    if (defined $color_scheme) {
	croak "Unknown color scheme: $color_scheme" unless defined $COLOR_SCHEMES{$color_scheme};
	$self->{COLOR_SCHEME} = $COLOR_SCHEMES{$color_scheme};
    } else {
	unless (defined $self->{COLOR_SCHEME}) {
	    $self->{COLOR_SCHEME} = $COLOR_SCHEMES{'Single color'};
	}
	return $self->{COLOR_SCHEME};
    }
}

=pod

=head2 Get color table

    $ct = $vector->get_color_table($create_allowed);

Returns the color table of this layer. Color tables (not in OGR)
can be used to specify individual colors for features. Color table
is an gdal::ColorTable object.

=cut

sub get_color_table {
    my($self,$create_allowed) = @_;
    return $self->{COLOR_TABLE} if $self->{COLOR_TABLE};
    if (!$self->{COLOR_TABLE} and $create_allowed) {
	$self->{COLOR_TABLE} = new gdal::ColorTable;
    }
    return $self->{COLOR_TABLE};
}

=pod

=head2 Bounding box

    @bb = $vector->world(feature=><feature_index>);

Returns the bounding box (minX,minY,maxX,maxY) of a feature
or the whole layer. Uses ogr::Geometry::GetEnvelope() or
ogr::Layer::GetExtent.

=cut

# in opt we may give feature=><feature_index>
sub world {
    my $self = shift;
    my %opt;
    %opt = @_ unless @_%2;
    my $extent;
    if (defined $opt{feature}) {
	my $f = $self->{ogr_layer}->GetFeature($opt{feature});
	croak "no such feature: $opt{feature} in $self->{filename}" unless $f;
	eval {
	    $extent = $f->GetGeometryRef()->GetEnvelope();
	};	
	if ($@) {
	    carp "GetEnvelope failed: ".$@;
	    return (0,0,1,1);
	}
    } else {
	eval {
	    $extent = $self->{ogr_layer}->GetExtent();
	};
	if ($@) {
	    carp "GetExtent failed: ".$@;
	    return (0,0,1,1);
	}
    }
    return ($extent->[0],$extent->[2],$extent->[1],$extent->[3]);
}

=pod

=head2 Rasterize

    $raster = $vector->rasterize(like=>$this, RenderAs=>$mode,
    feature=>$fid, value_field=>$fname);

Creates a new Geo::Raster, which has the size and extent of the
Geo::Raster $this and draws the layer on it. The raster is boolean
integer raster unless value_field is given. If value_field is floating
point value, the returned raster is a floating point raster. $mode is
optional, but if given either 1 (points), 2 (lines), or 3
(polygons). $fid (optional) is the number of the feature to render.

=cut

sub rasterize {
    my $self = shift;
    my %opt;

    %opt = @_ if @_;
    my %defaults = (RenderAs => $self->{RenderAs} ? $self->{RenderAs} : 0,
		    feature => -1,
		    nodata_value => -9999,
		    datatype => 'Integer');
    
    for (keys %defaults) {
	$opt{$_} = $defaults{$_} unless exists $opt{$_};
    }

    unless ($self->{OGRLayerH}) {
	croak "Geo::Vector->rasterize: empty layer";
    }

    unless ($opt{like} and ref($opt{like}) eq 'Geo::Raster') {
	croak "Geo::Vector->rasterize needs the option 'like=>\$raster'";
    }

    if (defined $opt{value_field} and $opt{value_field} ne '') {
	my $name = $opt{value_field};
	delete $opt{value_field};
	my $schema = $self->{ogr_layer}->GetLayerDefn();
	for my $i (0..$schema->GetFieldCount-1) {
	    my $column = $schema->GetFieldDefn($i);
	    if ($name eq $column->GetName) {
		my $type = $column->GetFieldTypeName($column->GetType);
		if ($type eq 'Integer' or $type eq 'Real') {
		    $opt{value_field} = $i;
		    $opt{datatype} = $type;
		    last;
		} else {
		    croak "rasterize: can't use value from field '$name' since its' type is '$type'";
		}
	    }
	}
	croak "rasterize: field with name '$name' does not exist" unless defined $opt{value_field};
    } else {
	$opt{value_field} = -1;
    }
	
    
    my $gd = new Geo::Raster datatype=>$opt{datatype},like=>$opt{like};
    $gd->nodata_value($opt{nodata_value});
    $gd->setall_nodata();

    xs_rasterize($self->{OGRLayerH},$gd->{GRID},$opt{RenderAs},$opt{feature},$opt{value_field});

    return $gd;
}

sub render {
    my($self, $pb, $alpha) = @_;
    $self->{Edit} = 0 unless defined $self->{Edit};
    $self->{RenderAs} = 0 unless defined $self->{RenderAs};
    $self->{PointSize} = 4 unless defined $self->{PointSize};
    my @c = @{$self->{COLOR}} if $self->{COLOR} and ref($self->{COLOR}) eq 'ARRAY';
    @c = (255,255,255,255) unless @c;
    $c[3] = $alpha if defined $alpha;

    my $color_scheme = $self->color_scheme;
    my $min = ($self->{PALETTE_MIN} or 0);
    my $max = ($self->{PALETTE_MAX} or 0);

    my $color_table = $self->get_color_table(1);

    my $value_field = -1;
    if (defined $self->{value_field} and $self->{value_field} ne '') {
	my $schema = $self->{ogr_layer}->GetLayerDefn();
	for my $i (0..$schema->GetFieldCount-1) {
	    my $column = $schema->GetFieldDefn($i);
	    if ($self->{value_field} eq $column->GetName) {
		my $type = $column->GetFieldTypeName($column->GetType);
		if ($type eq 'Integer') {
		    $value_field = $i;
		    last;
		} else {
		    croak "render vector: can't use value from field '$self->{value_field}' since its' type is '$type'";
		}
	    }
	}
	croak "render vector: field with name '$self->{value_field}' does not exist" if $value_field < 0;
    }

    ral_render_layer($pb, $self->{OGRLayerH}, @c, $self->{Edit}, $self->{RenderAs}, $self->{PointSize}, 
		     $color_scheme, $min, $max, $color_table, $value_field);
}

1;
__END__
=pod

=head1 SEE ALSO

gdal and ogr

This module should be discussed in geo-perl@list.hut.fi.

The homepage of this module is http://libral.sf.net.

=head1 AUTHOR

Ari Jolma, E<lt>ari.jolma at tkk.fiE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2005-2006 by Ari Jolma

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.5 or,
at your option, any later version of Perl 5 you may have available.

=cut

## @class Geo::Vector
# @brief A class for geospatial vector data
#
# Geo::Vector is an extra layer on top of Perl module ogr. ogr module
# allows using OGR (from GDAL) from Perl. Geo::Vector adds the required
# functionality to use ogr layers in Gtk2::Ex::Geo. Geo::Vector may also
# add extra functionality like rasterization, vectorization, rendering
# etc. which may be useful in stand-alone scripts.
# Geo::Vector mainly represents one layer but see below.
#
# This module should be discussed in geo-perl@list.hut.fi.
#
# The homepage of this module is http://libral.sf.net.
#
# @note All objects (for example Geo::Raster) given to methods or functions as 
# parameter and returned 
# from them are always given and returned as references (scalars) even if the 
# methods parameter or return type indicate the actual object type! 
# A reference to scalars, hashes and lists (arrays) is indicated by using for 
# the type names hashref, scalarref and listref!
#
# @see Geo::GDAL
#
# @author Ari Jolma
# @author Copyright (c) 2005-2006 by Ari Jolma
# @author This library is free software; you can redistribute it and/or modify
# it under the same terms as Perl itself, either Perl version 5.8.5 or,
# at your option, any later version of Perl 5 you may have available.
package Geo::Vector;

use 5.008;
use strict;
use warnings;
use Carp;
use File::Basename;
use POSIX;
POSIX::setlocale( &POSIX::LC_NUMERIC, "C" ); # http://www.remotesensing.org/gdal/faq.html nr. 11
use Geo::GDAL;
use Geo::Layer qw/:all/;
use Gtk2::Pango;
use Graph;

require Exporter;
use AutoLoader;

use vars qw( @ISA %GEOMETRY_TYPE %GEOMETRY_TYPE_INV %RENDER_AS );

@ISA = qw( Exporter Geo::Layer );

# Items to export into callers namespace by default. Note: do not export
# names by default without a very good reason. Use EXPORT_OK instead.
# Do not simply export all your public functions/methods/constants.

# This allows declaration	use Geo::Vector ':all';
# If you do not need this, moving things directly into @EXPORT or @EXPORT_OK
# will save memory.
our %EXPORT_TAGS = ( 'all' => [qw( %GEOMETRY_TYPE %RENDER_AS &drivers )] );

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

%GEOMETRY_TYPE = 
    (
     Unknown               => $Geo::OGR::wkbUnknown,
     Point                 => $Geo::OGR::wkbPoint,
     LineString            => $Geo::OGR::wkbLineString,
     Polygon               => $Geo::OGR::wkbPolygon,
     MultiPoint            => $Geo::OGR::wkbMultiPoint,
     MultiLineString       => $Geo::OGR::wkbMultiLineString,
     MultiPolygon          => $Geo::OGR::wkbMultiPolygon,
     GeometryCollection    => $Geo::OGR::wkbGeometryCollection,
     Point25D              => $Geo::OGR::wkbPoint25D,
     LineString25D         => $Geo::OGR::wkbLineString25D,
     Polygon25D            => $Geo::OGR::wkbPolygon25D,
     MultiPoint25D         => $Geo::OGR::wkbMultiPoint25D,
     MultiLineString25D    => $Geo::OGR::wkbMultiLineString25D,
     MultiPolygon25D       => $Geo::OGR::wkbMultiPolygon25D,
     GeometryCollection25D => $Geo::OGR::wkbGeometryCollection25D,
     );

for ( keys %GEOMETRY_TYPE ) {
    $GEOMETRY_TYPE_INV{ $GEOMETRY_TYPE{$_} } = $_;
}

# from ral_visual.h:
%RENDER_AS = ( Native => 0, Points => 1, Lines => 2, Polygons => 4 );

our $VERSION = '0.51';

## @ignore
sub AUTOLOAD {
    # This AUTOLOAD is used to 'autoload' constants from the constant()
    # XS function.
    
    my $constname;
    our $AUTOLOAD;
    ( $constname = $AUTOLOAD ) =~ s/.*:://;
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
XSLoader::load( 'Geo::Vector', $VERSION );

=pod

=head1 NAME

Geo::Vector - Perl extension for geospatial vectors

The documentation of Geo::Vector is in doxygen format

=cut

## @cmethod @geometry_types()
#
# @brief Returns a list of valid geometry types.
#
# %GEOMETRY_TYPE is a hash of standard type names: 'Point', 'LineString', ...
# @return a list of valid geometry types (as strings).
sub geometry_types {
    my ($class) = @_;
    return keys %GEOMETRY_TYPE;
}

## @cmethod @render_as_modes()
#
# @brief Returns a list of valid render as modes.
#
# %RENDER_AS is a hash of render types: 'Native', 'Points', ...
# @return a list of valid render as modes (as strings).
sub render_as_modes {
    my ($class) = @_;
    return keys %RENDER_AS;
}

## @cmethod %drivers()
#
# @brief Returns a hash of supported OGR drivers.
# @return a hash ( name => driver ) of supported OGR drivers
sub drivers {
    my %d;
    for (0..Geo::OGR::GetDriverCount()-1) {
	my $d = Geo::OGR::GetDriver($_);
	$d{$d->{name}} = $d;
    }
    return %d;
}

## @cmethod Geo::Vector new()
#
# @brief Creates an empty Geo::Vector object.
#
# The object can be used to host a link to a Geo::OGR::Datasource and a
# Geo::OGR::Layer in it, or it can be used to host in-memory Geo::OGR::Features.
#
# @return A new Geo::Vector object with default values
# @note This constructor inherits all the named parameters of Geo::Layer.

## @cmethod Geo::Vector new($filename)
#
# @brief Opens a Geo::Vector object from file.
#
# The object can be used to host a link to a Geo::OGR::Datasource and a
# Geo::OGR::Layer in it, or it can be used to host in-memory Geo::OGR::Features.
#
# You can simply open a geospatial vector file:
# @code
# $v = new Geo::Vector("borders.shp");
# @endcode
#
# @param[in] filename A file having parameters for the new object.
# @return A new Geo::Vector object with from file loaded values.
# @note This constructor inherits all the named parameters of Geo::Layer.

## @cmethod Geo::Vector new(%params)
#
# @brief Opens or creates a new Geo::Vector object with the given parameters.
#
# The object can be used to host a link to a Geo::OGR::Datasource and a
# Geo::OGR::Layer in it, or it can be used to host in-memory Geo::OGR::Features.
#
# You can open a geospatial vector file using named parameters:
#
# @code
# $v = new Geo::Vector(filename=>"borders.shp");
#
# $v = new Geo::Vector(datasource=>".", layer=>"borders.shp");
# @endcode
#
# @param[in] params is a list of named parameters:
# - filename => string, split into datasource and layer if given
# - datasource => string.
# - layer_name => string (optional). Name of the layer.
# - update => true/false (optional, default is false).
# - srs => number (optional). A geographic coordinate system code number in the
# EPSG database (European Petroleum Survey Group, http://www.epsg.org/). Default
# value is 4326, which is for WGS84.
# - geometry_type => one of geometry_type(), forwarded to Geo::OGR::CreateLayer,
# if a new layer is to be created.
# - sql => (optional) string, forwarded to Geo::OGR::ExecuteSQL if given.
# - schema => as in method schema (optional).
# - driver (optional), needed only if a new datasource is to be created.
# - driver_options (optional), forwarded to Geo::OGR::CreateDataSource.
# @return A new Geo::Vector object with attribute values given as parameter.
sub new {
    my $package = shift;
    my %params = @_ == 1 ? ( filename => $_[0] ) : @_;
    
    my %defaults = ( driver => '',
		     driver_options => [],
		     filename => '',
		     datasource => '',
		     layer_name => '',
		     update => 0,
		     srs => 'EPSG:4326',
		     geometry_type => 'Unknown',
		     sql => '',
		     );
    $params{sql} = $params{SQL} if $params{SQL};

    for my $key (keys %defaults) {
	next if defined $params{$key};
	$params{$key} = $defaults{$key};
    }
    $params{layer_name} = $params{layer} if exists $params{layer}; # support this still for a while

    ($params{layer_name},$params{datasource},undef) = fileparse($params{filename}) if $params{filename};

    my $self = Geo::Layer::new($package, %params);
    
    # open the datasource if datasource
    
    if ($params{datasource}) {
	if ($params{driver}) {
	    eval {
		if (ref($params{driver}) eq 'Geo::OGR::Driver') {
		    $self->{ogr_driver} = $params{driver};
		} elsif ($params{driver} =~ /^\d+$/) {
		    $self->{ogr_driver} = Geo::OGR::GetDriver($params{driver});
		} elsif ($params{driver}) {
		    $self->{ogr_driver} = Geo::OGR::GetDriverByName($params{driver});
		}
		croak "Can't find driver: $params{driver}" unless $self->{ogr_driver};
		$self->{ogr_datasource} = 
		    $self->{ogr_driver}->CreateDataSource($params{datasource}, $params{driver_options});
	    };
	    croak "CreateDataSource failed for $params{datasource}: $@" if $@;
	} else {
	    eval {
		$self->{ogr_datasource} = Geo::OGR::Open($params{datasource},$params{update});
	    };
	    croak "Geo::OGR::Open failed for $params{datasource}: $@" if $@;
	    croak "$params{datasource} could not be opened for update" 
		if $params{update} and !$self->{ogr_datasource};
	    $self->{update} = $params{update};
	}
	$self->{datasource} = $params{datasource};
	$self->{OGRDataSourceH} = get_OGRDataSourceH($self->{ogr_datasource}) 
	    if $self->{ogr_datasource};

	if ($params{layer_name} or $params{sql}) {
	    croak "Datasource $params{datasource} is empty and no driver has been defined." 
		unless $self->{ogr_datasource};
	    layer($self, %params);
	    $self->{OGRLayerH} = get_OGRLayerH($self->{ogr_layer});
	}
    } else {
	$self->{features} = [];
    }

    schema($self,$params{schema}) if $params{schema};

    # is blessed in Geo::Layer
    return $self;
}

## @method void save($filename)
#
# @brief Saving the object?
# @param[in] filename Name of file where the vector layer is saved.
# @todo The method is not yet ready!
sub save {
    my($self, $filename) = @_;
    
    # todo
    print STDERR "Geo::Vector object '"
	. $self->name()
	. "' was not saved because the save method is not implemented.";
}

## @method ref %layers()
#
# @brief Lists the layers that are available in the datasource to which this
# object points to.
# @return A hashref to a layer_name=>geometry_type hash.
sub layers {
    my ($self) = @_;
    my %layers;
    return {} unless $self->{ogr_datasource};
    for my $i ( 0 .. $self->{ogr_datasource}->GetLayerCount - 1 ) {
	my $l  = $self->{ogr_datasource}->GetLayerByIndex($i);
	my $fd = $l->GetLayerDefn();
	my $t  = $fd->GetGeomType;
	next unless $GEOMETRY_TYPE_INV{$t};
	$layers{ $l->GetName } = $GEOMETRY_TYPE_INV{$t};
    }
    return \%layers;
}

## @method void delete_layer($layer_name)
#
# @brief Attempts to Delete a layer from the datasource to which this object
# points to.
# @param[in] layer_name Name of the layer that should be deleted.
sub delete_layer {
    my ( $self, $layer_name ) = @_;
    return unless $self->{ogr_datasource};
    for my $i ( 0 .. $self->{ogr_datasource}->GetLayerCount - 1 ) {
	my $l = $self->{ogr_datasource}->GetLayerByIndex($i);
	$self->{ogr_datasource}->DeleteLayer($i), last
	    if $l->GetName() eq $layer_name;
    }
}

## @method Geo::Vector layer(%params)
#
# @brief Open or create a new layer.
# @param[in] params is a list of named parameters:
# - <I>sql</I> => string (optional) An SQL-string, forwarded to Geo::OGR::ExecuteSQL if
# given.
# - <I>layer_name</I> => string (optional). Name of/for the a layer.
# - <I>srs</I> => number (optional). A geographic coordinate system code number in the
# EPSG database (European Petroleum Survey Group, http://www.epsg.org/). Default
# value is 4326, which is for WGS84.
# - <I>geometry_type</I> => string (optional). One of the types supported given by
# geometry_type(), forwarded to Geo::OGR::CreateLayer if a new layer is to be
# created.
# @return An existing layer if one exists with the given name or can be found
# using the SQL command. Else the new created layer is returned.
# @exception If sql is given, but the SQL command is not possible to be executed.
# @exception If EPSG number is given for a new layer, but the number is not
# supported.
# @exception If the geometry type is given for a new layer, but it is not
# supported.
sub layer {
    my ( $self, %params ) = @_;
    return unless $self->{ogr_datasource};
    $params{layer_name} = $params{layer}
    if exists $params{layer};    # support this still for a while
    
    # Open by SQL
    if ( $params{sql} ) {
	$self->{sql} = $params{sql};
	eval {
	    $self->{ogr_layer} =
		$self->{ogr_datasource}->ExecuteSQL( $self->{sql} );
	};
	croak "ExecuteSQL failed: $@" if $@ or !$self->{ogr_layer};
	$self->{OGRLayerH} = get_OGRLayerH( $self->{ogr_layer} );
	return $self;
    }
    
    # Open by name
    $self->{ogr_layer} =
	$self->{ogr_datasource}->GetLayerByName( $params{layer_name} );
    if ( $self->{ogr_layer} ) {
	$self->{OGRLayerH} = get_OGRLayerH( $self->{ogr_layer} );
	return $self;
    }
    
    # Create new
    my $srs;
    if (ref($params{srs}) and ref($params{srs}) eq 'Geo::OSR::SpatialReference') {
	$srs = $params{srs};
    } else {
	$srs = new Geo::OSR::SpatialReference;
	$params{srs} = 'EPSG:4326' unless $params{srs};
	if ( $params{srs} =~ /^EPSG:(\d+)/ ) {
	    eval { $srs->ImportFromEPSG($1); };
	    croak "ImportFromEPSG failed: $@" if $@;
	} else {
	    croak "SRS $params{srs} not yet supported";
	}
    }
    my $t = exists $params{geometry_type} ? $params{geometry_type} : 'Unknown';
    croak "invalid geometry type: $GEOMETRY_TYPE{$t}"
	unless exists $GEOMETRY_TYPE{$t};
    eval {
	$self->{ogr_layer} =
	    $self->{ogr_datasource}
	->CreateLayer( $params{layer_name}, $srs, $GEOMETRY_TYPE{$t} );
    };
    croak "CreateLayer failed (does the datasource have update on?): $@"
	if $@ or !$self->{ogr_layer};
    $self->{update}    = 1;
    $self->{OGRLayerH} = get_OGRLayerH( $self->{ogr_layer} );
    return $self;
}

## @ignore
sub DESTROY {
    my $self = shift;
    return unless $self;
    $self->SUPER::DESTROY;
    if ( $self->{sql} and $self->{ogr_datasource} ) {
	$self->{ogr_datasource}->ReleaseResultSet( $self->{ogr_layer} );
    }
    if ( $self->{features} ) {
	for ( @{ $self->{features} } ) {
	    undef $_;
	}
    }
}

## @method $feature_count()
#
# @brief Returns the number of features in the layer (may be approximate) or in
# the object.
# @return Number of features in the layer (may be approximate) or in the object.
sub feature_count {
    my ($self) = @_;
    if ( $self->{features} ) {
	my $count = @{ $self->{features} };
	return $count;
    }
    return unless $self->{ogr_layer};
    my $count;
    eval { $count = $self->{ogr_layer}->GetFeatureCount(); };
    croak "GetFeatureCount failed: $@" if $@;
    return $count;
}
## @method Geo::OSR::SpatialReference srs(%params)
#
# @brief Get or set (set is not yet implemented) the spatial reference system of
# the layer.
#
# SRS (Spatial reference system) is a geographic coordinate system code number
# in the EPSG database (European Petroleum Survey Group, http://www.epsg.org/).
# Default value is 4326, which is for WGS84.
# @param[in] params (optional) is a list of named parameters:
# - format => string. Name of the wanted return format, like 'Wkt'. Wkt is for 
# Well-known text and is defined by the The OpenGIS Consortium specification for 
# the exchange (and easy persistance) of geometry data in ASCII format.
# @return Returns the current spatial reference system of the layer
# as a Geo::OSR::SpatialReference or wkt string.
sub srs {
    my ( $self, %params ) = @_;
    return unless $self->{ogr_layer};
    my $srs;
    eval { $srs = $self->{ogr_layer}->GetSpatialRef(); };
    croak "GetSpatialRef failed: $@" if $@;
    return unless $srs;
    if ( $params{format} ) {
	return $srs->ExportToWkt if $params{format} eq 'Wkt';
    }
    return $srs;
}

## @method $field_count()
#
# @brief For a layer object returns the number of fields in the layer schema.
# For a feature set object requires a named parameter that specifies the feature.
#
# Each feature in a feature set object may have its own schema.
# @return For a layer object returns the number of fields in the layer schema.
# For a feature set object requires a named parameter that specifies the feature.
sub field_count {
    my ( $self, %param ) = @_;
    if ( $self->{features} ) {
	my $f = $self->{features}->[ $param{feature} ];
	return $f ? $f->GetFieldCount() : undef;
    }
    return unless $self->{ogr_layer};
    my $n;
    eval { $n = $self->{ogr_layer}->GetLayerDefn()->GetFieldCount(); };
    croak "GetLayerDefn or GetFieldCount failed: $@" if $@;
    return $n;
}

## @method $geometry_type(%param)
#
# @brief For a layer object returns the geometry type of the layer.
# For a feature set object requires a named parameter that specifies the feature.
#
# @param[in] param is a list of named parameters:
# - flatten => true/false. Default is false.
# - feature => integer. Index of the feature whose geometry type is queried.
# @return For a layer object returns the geometry type of the layer.
# For a feature set object returns specified features geometry type.
sub geometry_type {
    my ( $self, %param ) = @_;
    my $t;
    if ( $self->{features} ) {
	my $f = $self->{features}->[ $param{feature} ];
	if ($f) {
	    $t = $f->GetGeometryRef->GetGeometryType;
	}
    }
    elsif ( $self->{ogr_layer} ) {
	$t = $self->{ogr_layer}->GetLayerDefn()->GetGeomType;
    }
    return unless $t;
    $t = $t & ~0x80000000 if $param{flatten};
    return $GEOMETRY_TYPE_INV{$t};
}

## @method hashref schema(ref % schema, ref Geo::OGR::Feature feature)
#
# @brief For a layer object gets or sets the schema of the layer.
# For a feature set object requires a named parameter that specifies the feature.
#
# @param[in] schema is a hashref field_name=>(Number, Type, TypeName, Justify,
# Width, Precision)=>value. So schema is a reference to a hash, whose keys are
# field names and values are hashrefs. The keys of the second hash are Number
# (o), Type (o), TypeName (i/o), and Justify, Width, and Precision (i/o, not
# obligatory). Fields are only created if they don't already exist in the layer.
# The returned schema contains a pseudofield FID (feature id).
# @param[in] feature The feature whose schema is queried (required for feature
# set layers)
# @return For a layer object returns a reference to the schema of the layer.
# For a feature object returns a reference to the specified features schema.
sub schema {
    my $self   = shift;
    my $schema = shift;
    my $feature;
    if ( ref($schema) ) {
	$feature = shift;
    }
    else {
	$feature = $schema;
	$schema  = undef;
    }
    my $s;
    if ( $self->{features} ) {
	$s = $self->{features}->[$feature]->GetDefnRef if $feature;
    }
    elsif ( $self->{ogr_layer} ) {
	$s = $self->{ogr_layer}->GetLayerDefn();
    }
    if ($schema) {
	croak "refusing to set the schema of all features in a feature table at once" unless $s;
	my %exists;
	my $n = $s->GetFieldCount();
	for my $i ( 0 .. $n - 1 ) {
	    my $fd   = $s->GetFieldDefn($i);
	    my $name = $fd->GetName;
	    $exists{$name} = 1;
	}
	for my $name ( keys %$schema ) {
	    $schema->{$name}{Number} = $n++
		unless defined $schema->{$name}{Number};
	}
	my $recreate = 0;
	for my $name ( sort { $schema->{$a}{Number} <=> $schema->{$b}{Number} }
		       keys %$schema ) {
	    next if $name eq 'FID';
	    my $d    = $schema->{$name};
	    my $type = $d->{Type};
	    $type = eval "\$Geo::OGR::OFT$d->{TypeName}" unless $type;
	    my $fd = new Geo::OGR::FieldDefn( $name, $type );
	    $fd->ACQUIRE;
	    $fd->SetJustify( $d->{Justify} ) if defined $d->{Justify};
	    $fd->SetWidth( $d->{Width} )     if defined $d->{Width};
	    
	    if ( exists $d->{Width} ) {
		$fd->SetWidth( $d->{Width} );
	    }
	    else {
		$fd->SetWidth(10) if $type == $Geo::OGR::OFTInteger;
	    }
	    $fd->SetPrecision( $d->{Precision} ) if defined $d->{Precision};
	    unless ( $exists{$name} ) {
		$self->{ogr_layer}->CreateField($fd) if $self->{ogr_layer};
		$recreate = 1;
	    }
	}
	if ( $self->{features} and $recreate ) {
	    my $sg = $s->GetGeometryRef();
	    my $dg = Geo::OGR::Geometry->new( $sg->GetGeometryType );
	    $dg->ACQUIRE;
	    copy_geometry_data( $sg, $dg );
	    my $d = Geo::OGR::FeatureDefn->new();
	    for my $name (
			  sort { $schema->{$a}{Number} <=> $schema->{$b}{Number} }
			  keys %$schema ) {
		my $fd =
		    Geo::OGR::FieldDefn->new( $name, $schema->{$name}{Type} );
		$fd->ACQUIRE;
		$fd->SetJustify( $schema->{$name}{Justify} )
		    if exists $schema->{$name}{Justify};
		if ( exists $schema->{$name}{Width} ) {
		    $fd->SetWidth( $schema->{$name}{Width} );
		}
		else {
		    $fd->SetWidth(10)
			if $schema->{$name}{Type} == $Geo::OGR::OFTInteger;
		}
		$fd->SetPrecision( $schema->{$name}{Precision} )
		    if exists $schema->{$name}{Precision};
		$d->AddFieldDefn($fd);
	    }
	    $d->DISOWN;    # this is given to feature
	    my $f = Geo::OGR::Feature->new($d);
	    $f->SetGeometry($dg);
	    for my $name ( keys %$schema ) {
		$f->SetField( $name, $s->GetField($name) ) if $exists{$name};
	    }
	    $self->{features}->[$feature] = $f;
	}
    } else {
	$schema = {};
	my @s;
	if ($s) {
	    push @s, $s;
	}
	else {
	    for my $feature ( @{ $self->{features} } ) {
		push @s, $feature->GetDefnRef;
	    }
	}
	eval {
	    for my $s (@s) {

		my $n = $s->GetFieldCount();
		
		# this data is potentially not ok if table
		for my $i ( 0 .. $n - 1 ) {
		    my $fd   = $s->GetFieldDefn($i);
		    my $name = $fd->GetName;
		    $schema->{$name}{Number}   = $i;
		    $schema->{$name}{Type}     = $fd->GetType;
		    $schema->{$name}{TypeName} =
			$fd->GetFieldTypeName( $fd->GetType );
		    $schema->{$name}{Justify}   = $fd->GetJustify;
		    $schema->{$name}{Width}     = $fd->GetWidth;
		    $schema->{$name}{Precision} = $fd->GetPrecision;
		}
	    }
	};
	croak "GetFieldCount failed: $@" if $@;
	$schema->{FID}{Number}   = -1;
	$schema->{FID}{Type}     = $Geo::OGR::OFTInteger;
	$schema->{FID}{TypeName} = 'Integer';
	return $schema;
    }
}

## @method select(%params)
#
# @brief Select features based on the information provided.
# @param params named params
# - <I>key</I> A Geo::OGR::Geometry object representing the point or area the user has selected
# The key, value pair is fed as such to features subroutine. 
# A call without parameters deselects all features.
sub select {
    my($self, %params) = @_;
    if (@_ > 1) {
	for my $key (keys %params) {
	    my $features = $self->features($key => $params{$key});
	    if ($features and @$features) {
		my $f = $self->selected_features();
		push @$features, values(%$f);
	    }
	    $self->selected_features($features);
	}
    } elsif ($params{selected_point}) {
	my $features = $self->features(that_contain => $params{selected_point});
	if ($features and @$features) {
	    my $f = $self->selected_features();
	    push @$features, values(%$f);
	}
	$self->selected_features($features);
    } elsif ($params{selected_area}) {
	my $features = $self->features(that_are_within => $params{selected_area});
	if ($features and @$features) {
	    my $f = $self->selected_features();
	    push @$features, values(%$f);
	}
	$self->selected_features($features);
    } else {
	$self->selected_features([]);
    }
}

## @method @value_range(%named_parameters)
#
# @brief Returns a list of the value range of the field.
# @param[in] named_parameters
# - field_name => string. The attribute whose min and max values are looked up.
# - filter => reference to a Geo::OGR::Geometry (optional). Used by 
# Geo::OGR::SetSpatialFilter() if the layer is an OGR layer.
# - filter_rect => reference to an array defining the rect (min_x, min_y, max_x, 
# max_y) (optional). Used by the Geo::OGR::SetSpatialFilterRect() if the layer 
# is an OGR layer.
# @return An array that has as it's first value the ranges minimum and as second
# the maximum -- array(min, max).

## @method @value_range($field_name)
#
# @brief Returns a list of the value range of the field.
# @param[in] field_name The name of the field, whose min and max values are 
# looked up.
# @return An array that has as it's first value the ranges minimum and as second
# the maximum -- array(min, max).
sub value_range {
    my $self = shift;
    my $field_name;
    my %param;
    if ( @_ == 1 ) {
	$field_name = shift;
    }
    else {
	%param      = @_;
	$field_name = $param{field_name};
    }
    
    if ( $self->{features} ) {
	my @range;
	for my $feature ( @{ $self->{features} } ) {
	    my $d = $feature->GetDefnRef;
	    my $n = $d->GetFieldCount;
	    my $value;
	    for my $i ( 0 .. $n - 1 ) {
		my $fd   = $d->GetFieldDefn($i);
		my $name = $fd->GetName;
		next unless $name eq $field_name;
		my $type = $fd->GetType;
		next
		    unless $type == $Geo::OGR::OFTInteger
		    or $type == $Geo::OGR::OFTReal;
		$value = $feature->GetField($i);
	    }
	    next unless defined $value;
	    $range[0] =
		defined $range[0]
		? ( $range[0] < $value ? $range[0] : $value )
		: $value;
	    $range[1] =
		defined $range[1]
		? ( $range[1] > $value ? $range[1] : $value )
		: $value;
	}
	return @range;
    }
    
    my $schema = $self->schema()->{$field_name};
    croak "value_range: field with name '$field_name' does not exist"
	unless defined $schema;
    croak
	"value_range: can't use value from field '$field_name' since its' type is '$schema->{TypeName}'"
	unless $schema->{TypeName} eq 'Integer'
	or $schema->{TypeName}     eq 'Real';
    
    return ( 0, $self->{ogr_layer}->GetFeatureCount - 1 )
	if $field_name eq 'FID';
    
    my $field = $schema->{Number};
    
# this would be probably faster as a database operation if data is in a database
    if ( exists $param{filter} ) {
	$self->{ogr_layer}->SetSpatialFilter( $param{filter} );
    }
    elsif ( exists $param{filter_rect} ) {
	$self->{ogr_layer}->SetSpatialFilterRect( @{ $param{filter_rect} } );
    }
    else {
	$self->{ogr_layer}->SetSpatialFilter(undef);
    }
    $self->{ogr_layer}->ResetReading();
    my @range;
    while (1) {
	my $f = $self->{ogr_layer}->GetNextFeature();
	last unless $f;
	my $value = $f->GetFieldAsString($field);
	$range[0] =
	    defined $range[0]
	    ? ( $range[0] < $value ? $range[0] : $value )
	    : $value;
	$range[1] =
	    defined $range[1]
		  ? ( $range[1] > $value ? $range[1] : $value )
		  : $value;
    }
    return @range;
}

## @method void copy_geometry_data(Geo::OGR::Geometry source, Geo::OGR::Geometry destination)
#
# @brief The method copies the data of the other Geo::OGR::Geometry to the other.
# @param[in] source A reference to an Geo::OGR::Geometry object, whose data is
# copied.
# @param[out] destination A reference to an Geo::OGR::Geometry object to which the
# other parameters data is copied to.
sub copy_geometry_data {
    my ( $src, $dst ) = @_;
    
    if ( $src->GetGeometryCount ) {
	
	for ( 0 .. $src->GetGeometryCount - 1 ) {
	    my $s = $src->GetGeometryRef($_);
	    my $t = $s->GetGeometryType;
	    my $n = $s->GetGeometryName;
	    $t = $Geo::OGR::wkbLinearRing if $n eq 'LINEARRING';
	    my $r = new Geo::OGR::Geometry($t);
	    $r->ACQUIRE;
	    copy_geom_data( $s, $r );
	    $dst->AddGeometry($r);
	}
	
    } else {
	
	for ( 0 .. $src->GetPointCount - 1 ) {
	    my $x = $src->GetX($_);
	    my $y = $src->GetY($_);
	    my $z = $src->GetZ($_);
	    $dst->AddPoint( $x, $y, $z );
	}
	
    }
}

## @method hashref has_field($field_name)
#
# @brief Tells if the layer has a field with a given name.
# @param[in] field_name Name of the field, which existence is asked.
# @return Returns the schema of the field.
sub has_field {
    my ( $self, $field_name ) = @_;
    return $self->schema->{$field_name};
}

sub selected_features {
    my($self, $selected) = @_;
    if (defined $selected) {
	if (ref $selected eq 'ARRAY') {
	    my %s = map { $_->GetFID => $_ } @$selected;
	    $self->{_selected} = \%s;
	} else {
	    $self->{_selected} = $selected;
	}
    } else {
	return $self->{_selected};
    }
}

## @method hashref feature($fid, $feature)
#
# @brief Get, add or update a feature.
#
# Example of retrieving:
# @code
# $feature = $vector->feature($i);
# @endcode
#
# Example of updating:
# @code
# $vector->feature($i, $feature);
# @endcode
#
# Example of adding:
# @code $vector->add_feature(%feature);
# @endcode
#
# @param[in] fid The FID of the feature (or the feature, if adding)
# @param[in] feature Feature to add (then no other parameters) or feature to update.
# @return Feature as a hashref (field_name, geometry, geometry_type)=>(value,
# geometry in array, geometry_type).
# @exception The fid is higher than the feature count.
# @todo Adding feature to ogr-layer.

## @method void feature(Geo::OGR::Feature feature)
#
# @brief Adding a feature.
#
# Example of adding:
# @code $vector->add_feature($feature);
# @endcode
#
# @param[in] feature Feature to add to the layer.
sub feature {
    my ( $self, $fid, $feature ) = @_;
    if ($feature) {
	
	# update at i
	if ( $self->{features} ) {
	    $feature = $self->make_feature($feature);
	    $self->{features}->[$fid] = $feature;
	}
	elsif ( $self->{ogr_layer} ) {
	    croak "can't set a feature in a layer (at least yet)";
	}
	else {
	    croak "no layer";
	}
    }
    elsif ( ref($fid) ) {
	$self->add_feature($fid);
    }
    else {
	
	# retrieve
	my $f;
	if ( $self->{features} ) {
	    $f = @{ $self->{features}->[$fid] };
	    croak "feature index out of bounds: $fid" unless $f;
	    
	}
	elsif ( $self->{ogr_layer} ) {
	    my $count = $self->{ogr_layer}->GetFeatureCount();
	    croak "Can't fetch feature nr $fid, there are only $count features"
		if $fid >= $count;
	    $f = $self->{ogr_layer}->GetFeature($fid);
	}
	else {
	    croak "no layer";
	}
	my $defn = $f->GetDefnRef;
	$feature = {};
	my $n = $defn->GetFieldCount();
	for my $fid ( 0 .. $n - 1 ) {
	    my $fd   = $defn->GetFieldDefn($fid);
	    my $name = $fd->GetName;
	    $feature->{$name} = $f->GetField($fid);
	}
	my $geometry = $f->GetGeometryRef;
	my $t        = $geometry->GetGeometryType;
	$feature->{geometry_type} = $GEOMETRY_TYPE_INV{$t};
	$feature->{geometry}      = get_geometry($geometry);
	return $feature;
    }
}

## @fn listref get_geometry(Geo::OGR::Geometry geometry)
#
# @brief Conversion Geo::OGR::Geometry -> Perl structure of arrays.
# @param[in] geometry An Geo::OGR::Geometry object.
# @return Reference to array having the geometry (for example for points:
# [x, y, z]).
sub get_geometry {
    my ($geometry) = @_;
    my $a = [];
    if ( $geometry->GetGeometryCount ) {
	for my $i ( 0 .. $geometry->GetGeometryCount - 1 ) {
	    push @$a, get_geometry( $geometry->GetGeometryRef($i) );
	}
    }
    elsif ( $geometry->GetPointCount == 1 ) {
	$a = [ $geometry->GetX(), $geometry->GetY(), $geometry->GetZ() ];
    }
    else {
	for my $i ( 0 .. $geometry->GetPointCount - 1 ) {
	    push @$a,
	    [ $geometry->GetX($i), $geometry->GetY($i), $geometry->GetZ($i) ];
	}
    }
    return $a;
}

## @fn void add_point(Geo::OGR::Geometry geometry, listref point)
#
# @brief Adds the given point to the given geometry.
# @param[out] geometry a Geo::OGR::Geometry object.
# @param[in] point A points coordinates as an array [x,y(,z)]
sub add_point {
    my ( $geometry, $point ) = @_;
    my $z = $point->[2];
    $z = 0 unless defined $z;
    $geometry->AddPoint( $point->[0], $point->[1], $z );
}

## @fn void add_points(Geo::OGR::Geometry geometry, listref points)
#
# @brief Adds the given points to the given geometry.
# @param[out] geometry A Geo::OGR::Geometry object.
# @param[in] points A reference to an array of points [point, point, ...], where
# each point is a reference to an array having the point's geometry [x,y(,z)].
sub add_points {
    my ( $geometry, $points ) = @_;
    for my $point (@$points) {
	add_point( $geometry, $point );
    }
}

## @fn Geo::OGR::Geometry make_geometry(listref geometry_array, $geometry_type)
#
# @brief Conversion from a Perl structure of arrays -> Geo::OGR::Geometry.
# @param[in] geometry_array A reference to an array having the geometry. In case of
# a single point the array has simply the point's coordinates ([x, y ,(z)]), but
# alternatively it can also have a set of references to other points (for
# example in the case of a line, multipoint or polygon). The geometry_array can
# be gotten by using the get_geometry() method.
# @param[in] geometry_type Type of the geometry. The geometry type can be gotten
# by using the geometry_type() method.
# @return A Geo::OGR::Geometry object.
sub make_geometry {
    my ( $a, $geometry_type ) = @_;
    $geometry_type = $GEOMETRY_TYPE{$geometry_type}
    unless exists $GEOMETRY_TYPE_INV{$geometry_type};
    my $geometry = new Geo::OGR::Geometry($geometry_type);
    $geometry->ACQUIRE;
    my $rt;
    if ( $geometry_type == $Geo::OGR::wkbPolygon ) {
	$rt = $Geo::OGR::wkbLinearRing;
    } elsif ( $geometry_type == $Geo::OGR::wkbPolygon25D ) {
	$rt = $Geo::OGR::wkbLinearRing25D;
    }
    my $m = $geometry_type & 0x80000000;
    $geometry_type = $geometry_type & ~0x80000000;
  SWITCH: {
      if ( $geometry_type == $Geo::OGR::wkbPoint ) {
	  add_point( $geometry, $a );
	  last SWITCH;
      }
      if ( $geometry_type == $Geo::OGR::wkbLineString ) {
	  add_points( $geometry, $a );
	  last SWITCH;
      }
      if ( $geometry_type == $Geo::OGR::wkbPolygon ) {
	  for my $ring (@$a) {
	      my $r = new Geo::OGR::Geometry($rt);
	      $r->ACQUIRE;
	      add_points( $r, $ring );
	      $geometry->AddGeometry($r);
	  }
	  last SWITCH;
      }
      if ( $geometry_type == $Geo::OGR::wkbMultiPoint ) {
	  my $mt = $m ? $Geo::OGR::wkbPoint25D : $geometry_type;
	  for my $b (@$a) {
	      my $p = new Geo::OGR::Geometry($mt);
	      $p->ACQUIRE;
	      add_point( $p, $b );
	      $geometry->AddGeometry($p);
	  }
	  last SWITCH;
      }
      if ( $geometry_type == $Geo::OGR::wkbMultiLineString ) {
	  my $mt = $m ? $Geo::OGR::wkbLineString25D : $geometry_type;
	  for my $b (@$a) {
	      my $i = new Geo::OGR::Geometry($mt);
	      $i->ACQUIRE;
	      add_points( $i, $b );
	      $geometry->AddGeometry($i);
	  }
	  last SWITCH;
      }
      if ( $geometry_type == $Geo::OGR::wkbMultiPolygon ) {
	  my $mt = $m ? $Geo::OGR::wkbPolygon25D : $geometry_type;
	  for my $b (@$a) {
	      my $i = new Geo::OGR::Geometry($mt);
	      $i->ACQUIRE;
	      for my $ring (@$b) {
		  my $r = new Geo::OGR::Geometry($rt);
		  $r->ACQUIRE;
		  add_points( $r, $ring );
		  $i->AddGeometry($r);
	      }
	      $geometry->AddGeometry($i);
	  }
	  last SWITCH;
      }
  }
    return $geometry;
}

## @method Geo::OGR::Feature make_feature(hashref feature)
#
# @brief Converts an feature given as hash to an Geo::OGR::Feature object.
# @param[in] feature is an reference to an hash having named parameters:
# - <I>fieldname</I>. Name of the field as a number (int/real) or string.
# - <I>geometry</I>. A reference to an array having the geometry. In case of
# a single point the array has simply the point's coordinates ([x, y ,(z)]), but
# alternatively it can also have a set of references to other points (for
# example in the case of a line, multipoint or polygon). The geometry_array can
# be gotten by using the get_geometry() method.
# - <I>geometry_type</I>. Type of the geometry. The geometry type can be gotten
# by using the geometry_type() method.
# @return Geo::OGR::Feature object.
# @note If the parameter is already a reference to an Geo::OGR::Feature nothing
# is done to the feature.

## @method Geo::OGR::Feature make_feature(%feature)
#
# @brief Converts an feature given as hash to an Geo::OGR::Feature object.
# @param[in] feature is a hash having named parameters:
# - <I>fieldname</I>. Name of the field as a number (int/real) or string.
# - <I>geometry</I>. A reference to an array having the geometry. In case of
# a single point the array has simply the point's coordinates ([x, y ,(z)]), but
# alternatively it can also have a set of references to other points (for
# example in the case of a line, multipoint or polygon). The geometry_array can
# be gotten by using the get_geometry() method.
# - <I>geometry_type</I>. Type of the geometry. The geometry type can be gotten
# by using the geometry_type() method.
# @return Geo::OGR::Feature object.
sub make_feature {
    my $self = shift;
    my $feature;
    my %feature;
    if ( @_ == 1 ) {
	$feature = shift;
	return $feature if ref($feature) eq 'Geo::OGR::Feature';
	if ( ref($feature) eq 'HASH' ) {
	    %feature = %{ $_[0] };
	}
	else {
	    croak "can't make a feature out of $feature";
	}
    }
    else {
	%feature = @_;
    }
    croak "you must specify a geometry for the new feature"
	unless $feature{geometry};
    my $defn;
    my $geom_type;
    
    if ( $self->{features} ) {
	$defn = Geo::OGR::FeatureDefn->new();
	for my $name ( sort keys %feature ) {
	    next if $name eq 'geometry' or $name eq 'geometry_type';
	    my $value = $feature{$name};    # fieldname found.
	    my $type;
	    if ( $value =~ /^[+-]*\d+$/ ) {
		$type = $Geo::OGR::OFTInteger;
	    }
	    elsif ( $value =~ /^[+-]*\d*\.*\d*$/ and $value =~ /\d/ ) {
		$type = $Geo::OGR::OFTReal;
	    }
	    else {
		$type = $Geo::OGR::OFTString;
	    }
	    my $fd = Geo::OGR::FieldDefn->new( $name, $type );
	    $fd->ACQUIRE;
	    $fd->SetWidth(10) if $type == $Geo::OGR::OFTInteger;
	    $defn->AddFieldDefn($fd);
	}
	$geom_type = $GEOMETRY_TYPE{ $feature{geometry_type} };
    }
    elsif ( $self->{ogr_layer} ) {
	$defn      = $self->{ogr_layer}->GetLayerDefn();
	$geom_type = $defn->GetGeomType;
	$geom_type = $GEOMETRY_TYPE{ $feature{geometry_type} }
	if $geom_type == $Geo::OGR::wkbUnknown;
    }
    else {
	croak "no features table nor layer where to put the new feature";
    }
    croak "could not come up with a type for the new geometry"
	unless $geom_type;
    
    $defn->DISOWN;    # feature owns
    $feature = Geo::OGR::Feature->new($defn);
    
    my $n = $defn->GetFieldCount();
    for my $i ( 0 .. $n - 1 ) {
	my $fd   = $defn->GetFieldDefn($i);
	my $name = $fd->GetName;
	$feature->SetField( $name, $feature{$name} );
    }
    
    $feature->SetGeometry( make_geometry( $feature{geometry}, $geom_type ) );
    
    return $feature;
}

## @method void add_feature(%feature)
#
# @brief Adds a feature to the layer.
# @param feature is a hash having named parameters:
# - <I>fieldname</I>. Name of the field as a number (int/real) or string.
# - <I>geometry</I>. A reference to an array having the geometry. In case of
# a single point the array has simply the point's coordinates ([x, y ,(z)]), but
# alternatively it can also have a set of references to other points (for
# example in the case of a line, multipoint or polygon). The geometry_array can
# be gotten by using the get_geometry() method.
# - <I>geometry_type</I>. Type of the geometry. The geometry type can be gotten
# by using the geometry_type() method.

## @method void add_feature(hashref feature)
#
# @brief Adds a feature to the layer.
# @param[in] feature is an reference to an hash having named parameters:
# - <I>fieldname</I>. Name of the field as a number (int/real) or string.
# - <I>geometry</I>. A reference to an array having the geometry. In case of
# a single point the array has simply the point's coordinates ([x, y ,(z)]), but
# alternatively it can also have a set of references to other points (for
# example in the case of a line, multipoint or polygon). The geometry_array can
# be gotten by using the get_geometry() method.
# - <I>geometry_type</I>. Type of the geometry. The geometry type can be gotten
# by using the geometry_type() method.

## @method void add_feature(Geo::OGR::Feature feature)
#
# @brief Adds a feature to the layer.
# @param feature A Geo::OGR::Feature object.
sub add_feature {
    my $self = shift;
    
    my $feature = $self->make_feature(@_);
    
    if ( $self->{features} ) {
	push @{ $self->{features} }, $feature;
    }
    elsif ( $self->{ogr_layer} ) {
	$self->{ogr_layer}->CreateFeature($feature);
	$self->{ogr_layer}->SyncToDisk;
    }

}

## @method listref features(%params)
#
# @brief Returns features satisfying the given requirement.
# @param[in] params is a list named parameters
# - <I>that_contain</I> => refeference to a list. A points coordinates as an array
# [x,y(,z)]. The point is given as requirement. A feature satisfies the
# requirement if the point is within the feature.
# - <I>that_intersect</I> => Geo::OGR::Geometry object. An envelope as requirement. A
# feature satisfies the requirement if the feature intersects with the given
# envelope.
# - <I>with_id</I> => Reference to an array of feature indexes (fids).
# @return A reference to an array of features.
sub features {
    my ( $self, %params ) = @_;
    my @features;
    if ( exists $params{that_contain} ) {
	my @point;
	my $point;
	if (ref($params{that_contain}) eq 'ARRAY') {
	    @point = @{ $params{that_contain} };
	    $point = new Geo::OGR::Geometry($Geo::OGR::wkbPoint);
	    $point->ACQUIRE;
	    $point->AddPoint(@point);
	} else {
	    $point = $params{that_contain};
	    @point = ($point->GetX, $point->GetY);
	}
	my $layer = $self->{ogr_layer};
	$layer->SetSpatialFilterRect( @point, @point );
	$layer->ResetReading();
	while (1) {
	    my $f = $layer->GetNextFeature();
	    last unless $f;
	    my $g = $f->GetGeometryRef;
	    next unless $point->Within($g);
	    push @features, $f;
	}
    }
    elsif ( exists $params{focus} ) {
	my $selected = $self->selected_features();
	my $layer = $self->{ogr_layer};
	$layer->SetSpatialFilterRect(@{$params{focus}});
	$layer->ResetReading();
	my $i = 1;
	for my $id (sort {$a<=>$b} keys %$selected) {
	    $i++;
	    push @features, $selected->{$id};
	}
	my $from = $params{from} || 1;
	my $count = $params{count} || 15;
	while ($i < $from+$count) {
	    my $f = $layer->GetNextFeature();
	    last unless $f;
	    next if $selected->{$f->GetFID};
	    $i++;
	    next if $i <= $from;
	    push @features, $f;
	}
    }
    elsif ( exists $params{filter_with_rect} ) {
	my $rect  = $params{filter_with_rect};
	my $layer = $self->{ogr_layer};
	$layer->SetSpatialFilterRect( @$rect );
	$layer->ResetReading();
	while (1) {
	    my $f = $layer->GetNextFeature();
	    last unless $f;
	    push @features, $f;
	}
    }
    elsif ( exists $params{that_are_within} ) {
	my $geom  = $params{that_are_within};
	my $layer = $self->{ogr_layer};
	my $e     = $geom->GetEnvelope;
	$layer->SetSpatialFilterRect( $e->[0], $e->[2], $e->[1], $e->[3] );
	$layer->ResetReading();
	while (1) {
	    my $f = $layer->GetNextFeature();
	    last unless $f;
	    my $g = $f->GetGeometryRef;
	    next unless $g->Within($geom);
	    push @features, $f;
	}
    }
    elsif ( exists $params{that_intersect} ) {
	my $geom  = $params{that_intersect};
	my $layer = $self->{ogr_layer};
	my $e     = $geom->GetEnvelope;
	$layer->SetSpatialFilterRect( $e->[0], $e->[2], $e->[1], $e->[3] );
	$layer->ResetReading();
	while (1) {
	    my $f = $layer->GetNextFeature();
	    last unless $f;
	    my $g = $f->GetGeometryRef;
	    next unless $g->Intersect($geom);
	    push @features, $f;
	}
    }
    elsif ( exists $params{with_id} ) {
	my $fids = $params{with_id};
	if ( $self->{features} ) {
	    for my $f (@$fids) {
		my $x = $self->{features}->[$f];
		push @features, $x if $x;
	    }
	}
	elsif ( $self->{ogr_layer} ) {
	    for my $f (@$fids) {
		my $x = $self->{ogr_layer}->GetFeature($f);
		push @features, $x if $x;
	    }
	}
	else {
	    croak "the layer contains no data";
	}
    }
    else {
	croak
	    "nothing to do! specify that_contain=>[x,y] or that_intersect=>\$geom";
    }
    return \@features;
}

## @method @world(hash params)
#
# @brief Get the bounding box (xmin, ymin, xmax, ymax) of the layer or one of
# its features.
#
# The method uses Geo::OGR::Geometry::GetEnvelope() or
# Geo::OGR::Layer::GetExtent().
#
# Example of getting a bounding box:
# @code
# @bb = $vector->world(feature=><feature_index>);
# @endcode
#
# @param[in] params is a list of named parameters:
# - feature => feature_index (optional).
# @return Returns the bounding box (minX, minY, maxX, maxY) as an array.
# If a single feature is defined with it's index as parameter, then the method
# returns that feature's bounding box, else the whole layer's bounding box.
sub world {
    my $self = shift;
    my %params;
    %params = @_ unless @_ % 2;
    my $extent;
    if ( defined $params{feature} ) {
	my $f;
	if ( $self->{features} ) {
	    $f = $self->{features}->[ $params{feature} ];
	} elsif ( $self->{ogr_layer} ) {
	    $f = $self->{ogr_layer}->GetFeature( $params{feature} );
	} else {
	    croak "no layer";
	}
	croak "feature with fid=$params{feature} does not exist" unless $f;
	eval { $extent = $f->GetGeometryRef()->GetEnvelope(); };
	croak "GetEnvelope failed: $@" if $@;
    }
    else {
	if ( $self->{features} ) {
	    for my $f ( @{ $self->{features} } ) {
		my $e = $f->GetGeometryRef()->GetEnvelope();
		unless ($extent) {
		    @$extent = @$e;
		}
		else {
		    $extent->[0] = MIN( $extent->[0], $e->[0] );
		    $extent->[2] = MIN( $extent->[2], $e->[2] );
		    $extent->[1] = MAX( $extent->[1], $e->[1] );
		    $extent->[3] = MAX( $extent->[3], $e->[3] );
		}
	    }
	}
	elsif ( $self->{ogr_layer} ) {
	    eval { $extent = $self->{ogr_layer}->GetExtent(); };
	    croak "GetExtent failed: $@" if $@;
	}
	else {
	    croak "no layer";
	}
    }
    
    # return a sensible world in any case
    unless ($extent) {
	$extent = [ 0, 1, 0, 1 ];
    }
    else {
	$extent->[1] = $extent->[0] + 1 if $extent->[1] <= $extent->[0];
	$extent->[3] = $extent->[2] + 1 if $extent->[3] <= $extent->[2];
    }
    return ( $extent->[0], $extent->[2], $extent->[1], $extent->[3] );
}

## @method Geo::Vector clip($name, %param)
#
# @brief Clip selected features from the layer into a new layer.
#
# @param[in] params is a list of named parameters:
# - <I>layer_name</I> name for the new layer (default is "clip")
# - <I>driver</I> driver (default is the driver of the layer)
# - <I>datasource</I> datasource (default is the datasource of the layer)
# The params are forwarded to the constructor of the new layer.
# @return A Geo::Vector object.
# @bug If self is a polygon shapefile, the result seems to be linestrings, but
# the saved shapefile is ok.
sub clip {
    my($self, %param) = @_;

    $param{clip} = 'clip' unless $param{clip};
    $param{datasource} = $self->{datasource} unless $param{datasource};
    $param{driver} = $self->{ogr_datasource}->GetDriver unless $param{driver};

    my $clip = new Geo::Vector(%param);

    my $schema = $self->{ogr_layer}->GetLayerDefn();

    for my $i (0..$schema->GetFieldCount-1) {
	my $fd = $schema->GetFieldDefn($i);
	$clip->{ogr_layer}->CreateField($fd);
    }

    my $features = $self->selected_features();
    
    for my $f (values %$features) {
	
	next unless $f; # should not happen
	
	my $geometry = $f->GetGeometryRef();
	
	# make copies of the features and add them to clip
	
	my $feature = new Geo::OGR::Feature($schema);
	$feature->SetGeometry($geometry); # makes a copy
	
	for my $i (0..$schema->GetFieldCount-1) {
	    my $value = $f->GetFieldAsString($i);
	    $feature->SetField($i, $value) if defined $value;
	}
	
	$clip->{ogr_layer}->CreateFeature($feature);
	
    }
    
    $clip->{ogr_layer}->SyncToDisk;

    return Geo::Vector->new(%param);
}

## @method void add_layer(Geo::Layer another)
#
# @brief Adds an another layer to this layer.
# @param[in] another An another Geo::Vector layer.
# @note The layers must have the same geometry_type.
sub add_layer {
    my ( $self, $another ) = @_;
    
    croak "the layer is not writable" unless $self->{update};

    my $type =
	$GEOMETRY_TYPE_INV{ $self->{ogr_layer}->GetLayerDefn->GetGeomType };
    my $another_type =
	$GEOMETRY_TYPE_INV{ $another->{ogr_layer}->GetLayerDefn->GetGeomType };
    
    croak "can't add a $another_type layer to a $type layer"
	unless $type eq $another_type;
    
    my $defn = $self->{ogr_layer}->GetLayerDefn();
    
    my $schema         = $self->schema;
    my $another_schema = $another->schema;
    
    $another->{ogr_layer}->ResetReading();
    
    while (1) {
	my $f = $another->{ogr_layer}->GetNextFeature();
	last unless $f;
	
	my $geometry = $f->GetGeometryRef();
	
	# make copies of the features and add them to self
	
	my $feature = new Geo::OGR::Feature($defn);
	$feature->SetGeometry($geometry);    # makes a copy
	
	for my $fn ( keys %$schema ) {
	    next if $fn eq 'FID';
	    next unless $another_schema->{$fn};
	    $feature->SetField( $fn, $f->GetFieldAsString($fn) );
	}
	$self->{ogr_layer}->CreateFeature($feature);
    }
    $self->{ogr_layer}->SyncToDisk;
}

## @method Geo::Raster rasterize(%named_params)
#
# @brief Creates a new Geo::Raster from this Geo::Vector object.
#
# The new Geo::Raster has the size and extent of the Geo::Raster $this and draws
# the layer on it. The raster is boolean integer raster unless value_field is
# given. If value_field is floating point value, the returned raster is a
# floating point raster. render_as hash is optional, but if given should be one of
# 'Native', 'Points', 'Lines', or 'Polygons'. $fid (optional) is the number of
# the feature to render.
#
# @param[in] like (optional). A Geo::Raster object, from which the resulting
# Geo::Raster object's size and extent are copied.
# @param[in] M (optional). Height of the resulting Geo::Raster object. Has to be
# given if hash like is not given. If like is given, then M will not be used.
# @param[in] N (optional). Width of the resulting Geo::Raster object. Has to be
# given if hash like is not given. If like is given, then N will not be used.
# @param[in] world (optional). The world (bounding box) of the resulting raster
# layer. Useless to give if parameter like is given, because then it's world
# will be used.
# @param[in] render_as (optional). Rendering mode, which should be 'Native',
# 'Points', 'Lines' or 'Polygons'.
# @param[in] feature (optional). Number of the feature to render.
# @param[in] value_field (optional). Value fields name.
# @return A new Geo::Raster, which has the size and extent of the given as
# parameters and values of this object's OGRLayerH.
sub rasterize {
    my $self = shift;
    my %params;
    
    %params = @_ if @_;
    
    my %defaults = (
		    render_as => $self->{RENDER_AS} ? $self->{RENDER_AS} : 'Native',
		    feature => -1,
		    nodata_value => -9999,
		    datatype     => 'Integer'
		    );

    for ( keys %defaults ) {
	$params{$_} = $defaults{$_} unless exists $params{$_};
    }
    
    croak "Not a valid rendering mode: $params{render_as}" unless defined $RENDER_AS{$params{render_as}};
    
    croak "Geo::Vector->rasterize: empty layer" unless $self->{OGRLayerH};
    
    ( $params{M}, $params{N} ) = $params{like}->size() if $params{like};
    $params{world} = [ $params{like}->world() ] if $params{like};
    
    croak "Geo::Vector->rasterize needs the raster size: M, N"
	unless $params{M} and $params{N};
    
    $params{world} = [ $self->world() ] unless $params{world};
    
    my $field = -1;
    if ( defined $params{value_field} and $params{value_field} ne '' ) {
	my $schema = $self->schema()->{ $params{value_field} };
	croak "rasterize: field with name '$params{value_field}' does not exist"
	    unless defined $schema;
		croak
		    "rasterize: can't use value from field ".
		    "'$params{value_field}' since its' type is '$schema->{TypeName}'"
		    unless $schema->{TypeName} eq 'Integer'
		    or $schema->{TypeName}     eq 'Real';
	$field = $schema->{Number};
	$params{datatype} = $schema->{TypeName};
    }
    
    my $gd = Geo::Raster->new(
			      datatype => $params{datatype},
			      M        => $params{M},
			      N        => $params{N},
			      world    => $params{world}
			      );
    $gd->nodata_value( $params{nodata_value} );
    $gd->set('nodata');
    
    xs_rasterize( $self->{OGRLayerH}, $gd->{GRID},
		  $RENDER_AS{ $params{render_as} },
		  $params{feature}, $field );
    
    return $gd;
}

## @method void graph()
#
# @brief Creates an undirected weighted graph from the OGR-layer.
#
# The edge weights are calculated according to the distances between the 
# points inside each layer's feature. Requires Geo::Distance.
sub graph {
    my ($self) = @_;
    my %node;
    my %edge;
    my $layer = $self->{ogr_layer};
    $layer->ResetReading();
    my $distance = Geo::Distance->new();
    while ( my $feature = $layer->GetNextFeature() ) {
	
	my $geom = $feature->GetGeometryRef();
	my $n    = $geom->GetPointCount - 1;
	
	my $length = 0;
	
	# length of linestring computation, this is lat lon
	my $lon1 = $geom->GetX(0);
	my $lat1 = $geom->GetY(0);
	for my $i ( 1 .. $n ) {
	    my $lon2 = $geom->GetX($i);
	    my $lat2 = $geom->GetY($i);
	    $length +=
		$distance->distance( 'meter', $lon1, $lat1, $lon2, $lat2 );
	    $lon1 = $lon2;
	    $lat1 = $lat2;
	}
	
	# the cost model
	my $cost = $length;
	
	if (    $geom->GetGeometryCount == 0
		and $geom->GetGeometryName =~ /^linestring/i ) {
	    
	    my $fid = $feature->GetFID();
	    
	    # the accuracy
	    my $first = sprintf( "%.5f %.5f", $geom->GetX(0), $geom->GetY(0) );
	    my $last = sprintf( "%.5f %.5f", $geom->GetX($n), $geom->GetY($n) );
	    
	    $node{$first}++;
	    $node{$last}++;
	    
	    # edges
	    my $liikennevi = $feature->GetFieldAsString('LIIKENNEVI');
	    if ( $liikennevi == 2 ) {
		$edge{$first}{$last} = $cost;
		$edge{$last}{$first} = $cost;
	    }
	    elsif ( $liikennevi == 3 ) {
		$edge{$last}{$first} = $cost;
	    }
	    elsif ( $liikennevi == 4 ) {
		$edge{$first}{$last} = $cost;
	    }
	}
    }
    
    my $g = Graph->new;
    for my $u ( keys %node ) {
	$g->add_vertex($u);
	for my $v ( keys %{ $edge{$u} } ) {
	    $g->add_weighted_edge( $u, $v, $edge{$u}{$v} );
	}
    }
    $self->{graph} = $g;
}

## @method void overlay_graph(Gtk2::Gdk::Pixmap pixmap)
#
# @brief Creates from the objects graph an overlay graph (incl. vertices and 
# edges) as a pixmap.
# @param[in,out] pixmap Gtk2::Gdk::Pixmap
sub overlay_graph {
    my ( $self, $pixmap ) = @_;
    my @V  = $self->{graph}->vertices;
    my $gc = new Gtk2::Gdk::GC $pixmap;
    $gc->set_rgb_fg_color( Gtk2::Gdk::Color->new( 65535, 65535, 0 ) );
    for my $v (@V) {
	my @p = split /\s+/, $v;
	@p = $self->{overlay}->point2pixmap_pixel(@p);
	$pixmap->draw_rectangle( $gc, 0, $p[0] - 2, $p[1] - 2, 5, 5 );
    }
    my @E = $self->{graph}->edges;
    for my $e (@E) {
	my @u = split /\s+/, $e->[0];
	my @v = split /\s+/, $e->[1];
	@u = $self->{overlay}->point2pixmap_pixel(@u);
	@v = $self->{overlay}->point2pixmap_pixel(@v);
	$pixmap->draw_line( $gc, @u, @v );
	next;
	
	# arrows.. not very helpful
	my $deltaX = $v[0] - $u[0];
	my $deltaY = $v[1] - $u[1];
	my $theta  =
	    $deltaX == 0 ? 3.14159 / 2.0 : POSIX::atan( $deltaY / $deltaX );
	my $theta2 = $theta;
	$theta2 += 3.14159 if $deltaX < 0;
	my $lengthdeltaX = -cos($theta2) * 8;
	my $lengthdeltaY = -sin($theta2) * 8;
	my $widthdeltaX  = sin($theta2) * 5;
	my $widthdeltaY  = cos($theta2) * 5;
	$pixmap->draw_line(
			   $gc, @v,
			   int( $v[0] + $lengthdeltaX + $widthdeltaX ),
			   int( $v[1] + $lengthdeltaY - $widthdeltaY )
			   );
	$pixmap->draw_line(
			   $gc, @v,
			   int( $v[0] + $lengthdeltaX - $widthdeltaX ),
			   int( $v[1] + $lengthdeltaY + $widthdeltaY )
			   );
    }
}

1;
__END__

=pod

=head1 SEE ALSO

Geo::GDAL

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

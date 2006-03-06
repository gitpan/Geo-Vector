#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#include "ppport.h"

#include <ral_grid.h>
#include <ral_pixbuf.h>

#include "const-c.inc"

IV SV2Handle(SV *sv)
{
	if (SvGMAGICAL(sv))
		mg_get(sv);
	if (!sv_isobject(sv)) {
		croak("dataset is not an object");
	}
	SV *tsv = (SV*)SvRV(sv);
	IV tmp;
	if ((SvTYPE(tsv) != SVt_PVHV)) {
		croak("dataset is not a hashref");
	}
	if (!SvMAGICAL(tsv)) {
		croak("dataset does not have magic");
	}
	MAGIC *mg = mg_find(tsv,'P');
	if (!mg) {
		croak("dataset does not have right kind of magic");
	}
	sv = mg->mg_obj;
	if (!sv_isobject(sv)) {
		croak("dataset does not have really right kind of magic");
	}

	tmp = SvIV((SV*)SvRV(sv));
	return tmp;
}

MODULE = Geo::Vector		PACKAGE = Geo::Vector		

INCLUDE: const-xs.inc

OGRDataSourceH
get_OGRDataSourceH(ogr)
	SV *ogr
	CODE:
	{
		OGRDataSourceH h = (OGRDataSourceH)0;
		IV tmp = SV2Handle(ogr);
		h = (OGRDataSourceH)tmp;
		RETVAL = h;
	}
  OUTPUT:
    RETVAL

OGRLayerH
get_OGRLayerH(layer)
	SV *layer
	CODE:
	{
		OGRLayerH h = (OGRLayerH)0;
		IV tmp = SV2Handle(layer);
		h = (OGRLayerH)tmp;
		RETVAL = h;
	}
  OUTPUT:
    RETVAL

int
xs_rasterize(l, gd, render_override, fid_to_rasterize, value_field)
	OGRLayerH l
	ral_grid *gd
	int render_override
	int fid_to_rasterize
	int value_field
	CODE:
	if (fid_to_rasterize > -1 ) {

		OGRFeatureH f = OGR_L_GetFeature(l, fid_to_rasterize);
		OGRFieldType ft = OFTInteger;

		if (value_field > -1)
			ft = OGR_Fld_GetType(OGR_FD_GetFieldDefn(OGR_L_GetLayerDefn(l), value_field));

		ral_rasterize_feature(gd, f, value_field, ft, render_override);

	} else {

		ral_rasterize_layer(gd, l, value_field, render_override);
	}

	RETVAL = 1;

  OUTPUT:
    RETVAL
	POSTCALL:
		if (ral_has_msg())
			croak(ral_get_error_msg());

void
ral_render_layer(pb, l, c1, c2, c3, alpha, show_points, render_override, point_size, color_scheme, min, max, color_table, value_field)
	ral_pixbuf *pb
	OGRLayerH l
	int c1
	int c2
	int c3
	int alpha
	int show_points
	int render_override
	int point_size
	int color_scheme
	double min
	double max
	SV *color_table
	int value_field

	CODE:
	ral_hash *selected_points = NULL;
	GDALColorEntry clr = {c1, c2, c3, alpha};
	GDALColorTableH ctH;
	ctH = (GDALColorTableH)SV2Handle(color_table);	
	if (ctH)
		ral_render_layer(pb, l, clr, selected_points, render_override, point_size, color_scheme, min, max, ctH, value_field);

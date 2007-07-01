#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#include "ppport.h"

#include <ral.h>

#include "const-c.inc"

IV SV2Handle(SV *sv)
{
	if (SvGMAGICAL(sv))
		mg_get(sv);
	if (!sv_isobject(sv))
		croak("parameter is not an object");
	SV *tsv = (SV*)SvRV(sv);
	if ((SvTYPE(tsv) != SVt_PVHV))
		croak("parameter is not a hashref");
	if (!SvMAGICAL(tsv))
		croak("parameter does not have magic");
	MAGIC *mg = mg_find(tsv,'P');
	if (!mg)
		croak("parameter does not have right kind of magic");
	sv = mg->mg_obj;
	if (!sv_isobject(sv))
		croak("parameter does not have really right kind of magic");
	return SvIV((SV*)SvRV(sv));
}

GDALColorEntry fetch_color(AV *a, int i)
{
	GDALColorEntry color;
	SV **s = av_fetch(a, i++, 0);
	color.c1 = s ? SvUV(*s) : 0;
	s = av_fetch(a, i++, 0);
	color.c2 = s ? SvUV(*s) : 0;
	s = av_fetch(a, i++, 0);
	color.c3 = s ? SvUV(*s) : 0;
	s = av_fetch(a, i++, 0);
	color.c4 = s ? SvUV(*s) : 0;
	return color;
}

#define RAL_FETCH(from, key, to, as) \
{SV **s = hv_fetch(from, key, strlen(key), 0);\
 if (s) {\
	(to) = as(*s);\
}}

#define RAL_STORE(to, key, from, with) \
hv_store(to, key, strlen(key), with(from), 0);

int fetch2visual(HV *perl_layer, ral_visual *visual, OGRFeatureDefnH defn)
{
	/* these are mostly from the Geo::Layer object */
	RAL_FETCH(perl_layer, "ALPHA", visual->alpha, SvIV);
	RAL_FETCH(perl_layer, "PALETTE_VALUE", visual->palette_type, SvIV);
	RAL_FETCH(perl_layer, "SYMBOL_VALUE", visual->symbol, SvIV);
	RAL_FETCH(perl_layer, "SYMBOL_SIZE", visual->symbol_pixel_size, SvIV);
	RAL_FETCH(perl_layer, "HUE_AT_MIN", visual->hue_at.min, SvIV);
	RAL_FETCH(perl_layer, "HUE_AT_MAX", visual->hue_at.max, SvIV);
	RAL_FETCH(perl_layer, "HUE_DIR", visual->hue_dir, SvIV);
	RAL_FETCH(perl_layer, "HUE", visual->hue, SvIV);
	SV **s = hv_fetch(perl_layer, "SINGLE_COLOR", strlen("SINGLE_COLOR"), 0);
	if (s AND SvROK(*s)) {
		AV *a = (AV*)SvRV(*s);
		if (a)
			visual->single_color = fetch_color(a, 0);
	}
	RAL_FETCH(perl_layer, "SYMBOL_FIELD_VALUE", visual->symbol_field, SvIV);
	OGRFieldType symbol_field_type;
	if (visual->symbol_field >= 0) {
		RAL_CHECK(ral_get_field_type(defn, visual->symbol_field, &symbol_field_type));
	} else /* FID or fixed size */
		symbol_field_type = OFTInteger;

	switch (symbol_field_type) {
	case OFTInteger:
		RAL_FETCH(perl_layer, "SYMBOL_SCALE_MIN", visual->symbol_size_int.min, SvIV);
		RAL_FETCH(perl_layer, "SYMBOL_SCALE_MAX", visual->symbol_size_int.max, SvIV);
		break;
	case OFTReal:
		RAL_FETCH(perl_layer, "SYMBOL_SCALE_MIN", visual->symbol_size_double.min, SvNV);
		RAL_FETCH(perl_layer, "SYMBOL_SCALE_MAX", visual->symbol_size_double.max, SvNV);
		break;
	default:
		RAL_CHECKM(0, ral_msg("Invalid field type for symbol scale: %s", OGR_GetFieldTypeName(symbol_field_type)));
		break;
	}

	RAL_FETCH(perl_layer, "COLOR_FIELD_VALUE", visual->color_field, SvIV);
	OGRFieldType color_field_type;
	if (visual->color_field >= 0) {
		RAL_CHECK(ral_get_field_type(defn, visual->color_field, &color_field_type));
	} else /* FID */
		color_field_type = OFTInteger;

	switch (color_field_type) {
	case OFTInteger:
		RAL_FETCH(perl_layer, "COLOR_SCALE_MIN", visual->color_int.min, SvIV);
		RAL_FETCH(perl_layer, "COLOR_SCALE_MAX", visual->color_int.max, SvIV);
		break;
	case OFTReal:
		RAL_FETCH(perl_layer, "COLOR_SCALE_MIN", visual->color_double.min, SvNV);
		RAL_FETCH(perl_layer, "COLOR_SCALE_MAX", visual->color_double.max, SvNV);
		break;
	case OFTString:
		break;
	default:
		RAL_CHECKM(0, ral_msg("Invalid field type for color scale: %s", OGR_GetFieldTypeName(color_field_type)));
		break;
	}
	
	RAL_FETCH(perl_layer, "RENDER_AS_VALUE", visual->render_as, SvIV);
	s = hv_fetch(perl_layer, "COLOR_TABLE", strlen("COLOR_TABLE"), 0);
	if (visual->palette_type == RAL_PALETTE_COLOR_TABLE AND s AND SvROK(*s)) {
		AV *a = (AV*)SvRV(*s);
		int i, n = a ? av_len(a)+1 : 0;
		if (n > 0) {
			switch (color_field_type) {
			case OFTInteger:
				RAL_CHECK(visual->color_table = ral_color_table_create(n));
				for (i = 0; i < n; i++) {
					SV **s = av_fetch(a, i, 0);
					AV *c;
					RAL_CHECKM(s AND SvROK(*s) AND (c = (AV*)SvRV(*s)), "Bad color table data");
					s = av_fetch(c, 0, 0);
					visual->color_table->keys[i] = s ? SvIV(*s) : 0;
					visual->color_table->colors[i] = fetch_color(c, 1);
				}
				break;
			case OFTString:
				RAL_CHECK(visual->string_color_table = ral_string_color_table_create(n));
				for (i = 0; i < n; i++) {
					STRLEN len;
					SV **s = av_fetch(a, i, 0);
					AV *c;
					RAL_CHECKM(s AND SvROK(*s) AND (c = (AV*)SvRV(*s)), "Bad color table data");
					s = av_fetch(c, 0, 0);
					if (s)
						ral_string_color_table_set(visual->string_color_table, SvPV(*s, len), i, fetch_color(c, 1));
				}
				break;
			default:
    				RAL_CHECKM(0, ral_msg("Invalid field type for color table: %s", OGR_GetFieldTypeName(color_field_type)));
			}
		}
	}
	s = hv_fetch(perl_layer, "COLOR_BINS", strlen("COLOR_BINS"), 0);
	if (visual->palette_type == RAL_PALETTE_COLOR_BINS AND s AND SvROK(*s)) {
		AV *a = (AV*)SvRV(*s);
		int i, n = a ? av_len(a)+1 : 0;
		if (n > 0) {
			switch (color_field_type) {
			case OFTInteger:
    				RAL_CHECK(visual->int_bins = ral_int_color_bins_create(n));
				for (i = 0; i < n; i++) {
					SV **s = av_fetch(a, i, 0);
					AV *c;
					RAL_CHECKM(s AND SvROK(*s) AND (c = (AV*)SvRV(*s)), "Bad color bins data");
					s = av_fetch(c, 0, 0);
					if (i < n-1)
						visual->int_bins->bins[i] = s ? SvIV(*s) : 0;
					visual->int_bins->colors[i] = fetch_color(c, 1);
				}
			    	break;
			case OFTReal:
				RAL_CHECK(visual->double_bins = ral_double_color_bins_create(n));
				for (i = 0; i < n; i++) {
					SV **s = av_fetch(a, i, 0);
					AV *c;
					RAL_CHECKM(s AND SvROK(*s) AND (c = (AV*)SvRV(*s)), "Bad color bins data");
					s = av_fetch(c, 0, 0);
					if (i < n-1)
						visual->double_bins->bins[i] = s ? SvNV(*s) : 0;
					visual->double_bins->colors[i] = fetch_color(c, 1);
				}
				break;
			default:
				RAL_CHECKM(0, ral_msg("Invalid field type for color bins: %s", OGR_GetFieldTypeName(color_field_type)));
			}
		}
	}
	return 1;
	fail:
	return 0;
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

void
xs_rasterize(l, gd, render_override, fid_to_rasterize, value_field)
	OGRLayerH l
	ral_grid *gd
	int render_override
	int fid_to_rasterize
	int value_field
	CODE:
	if (fid_to_rasterize > -1 ) {

		OGRFieldType ft = 0;
		OGRFeatureH f = OGR_L_GetFeature(l, fid_to_rasterize);
		if (value_field >= 0)
			RAL_CHECK(ral_get_field_type(l, value_field, &ft));
		ral_grid_rasterize_feature(gd, f, value_field, ft, render_override);

	} else {

		ral_grid_rasterize_layer(gd, l, value_field, render_override);
	}
	fail:
	POSTCALL:
		if (ral_has_msg())
			croak(ral_get_msg());

ral_visual_layer *
ral_visual_layer_create(perl_layer, ogr_layer)
	HV *perl_layer
	OGRLayerH ogr_layer
	CODE:
		ral_visual_layer *layer = ral_visual_layer_create();
		layer->layer = ogr_layer;
		RAL_CHECK(fetch2visual(perl_layer, &layer->visualization, OGR_L_GetLayerDefn(layer->layer)));
		RAL_FETCH(perl_layer, "EPSG_FROM", layer->EPSG_from, SvIV);
		RAL_FETCH(perl_layer, "EPSG_TO", layer->EPSG_to, SvIV);
		goto ok;
		fail:
		ral_visual_layer_destroy(&layer);
		layer = NULL;
		ok:
		RETVAL = layer;
  OUTPUT:
    RETVAL
	POSTCALL:
		if (ral_has_msg())
			croak(ral_get_msg());

void
ral_visual_layer_destroy(layer)
	ral_visual_layer *layer
	CODE:
		ral_visual_layer_destroy(&layer);

void
ral_visual_layer_render(layer, pb)
	ral_visual_layer *layer
	ral_pixbuf *pb
	CODE:
		ral_render_visual_layer(pb, layer);
	POSTCALL:
	if (ral_has_msg())
		croak(ral_get_msg());

ral_visual_feature_table *
ral_visual_feature_table_create(perl_layer, features)
	HV *perl_layer
	AV *features
	CODE:
		ral_visual_feature_table *layer = ral_visual_feature_table_create(av_len(features)+1);
		RAL_CHECK(layer);
		char *color_field_name = NULL, *symbol_size_field_name = NULL;;

		RAL_FETCH(perl_layer, "COLOR_FIELD", color_field_name, SvPV_nolen);
		RAL_FETCH(perl_layer, "SYMBOL_FIELD", symbol_size_field_name, SvPV_nolen);

		int i;
		for (i = 0; i <= av_len(features); i++) {
			SV** sv = av_fetch(features,i,0);
			OGRFeatureH f = SV2Handle(*sv);
			layer->features[i].feature = f;
			OGRFeatureDefnH fed = OGR_F_GetDefnRef(f);

			int field = -1;
			if (color_field_name) {
				field = OGR_FD_GetFieldIndex(fed, color_field_name);
				if (field >= 0) {
					OGRFieldDefnH fid = OGR_FD_GetFieldDefn(fed, field);
					OGRFieldType fit = OGR_Fld_GetType(fid);
					if (!(fit == OFTInteger OR fit == OFTReal))
						field = -1;
				}
			}
			RAL_STORE(perl_layer, "COLOR_FIELD_VALUE", field, newSViv);

			field = -2;
			if (symbol_size_field_name) {
				field = OGR_FD_GetFieldIndex(fed, symbol_size_field_name);
				if (field >= 0) {
					OGRFieldDefnH fid = OGR_FD_GetFieldDefn(fed, field);
					OGRFieldType fit = OGR_Fld_GetType(fid);
					if (!(fit == OFTInteger OR fit == OFTReal))
						field = -2;
				} else
					field = -2;
			}
			RAL_STORE(perl_layer, "SYMBOL_FIELD_VALUE", field, newSViv);

			RAL_CHECK(fetch2visual(perl_layer, &layer->features[i].visualization, OGR_F_GetDefnRef(f)));
			
		}

		RAL_FETCH(perl_layer, "EPSG_FROM", layer->EPSG_from, SvIV);
		RAL_FETCH(perl_layer, "EPSG_TO", layer->EPSG_to, SvIV);
		goto ok;
		fail:
		ral_visual_feature_table_destroy(&layer);
		layer = NULL;
		ok:
		RETVAL = layer;
  OUTPUT:
    RETVAL
	POSTCALL:
		if (ral_has_msg())
			croak(ral_get_msg());

void
ral_visual_feature_table_destroy(layer)
	ral_visual_feature_table *layer
	CODE:
		ral_visual_feature_table_destroy(&layer);

void
ral_visual_feature_table_render(layer, pb)
	ral_visual_feature_table *layer
	ral_pixbuf *pb
	CODE:
		ral_render_visual_feature_table(pb, layer);
	POSTCALL:
	if (ral_has_msg())
		croak(ral_get_msg());


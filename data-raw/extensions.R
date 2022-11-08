gpkg_extensions <-
    tibble::tribble(
    ~pos,                             ~extension,                                                                             ~url,
      1L,            "Non-Linear Geometry Types",    "http://www.geopackage.org/guidance/extensions/nonlinear_geometry_types.html",
      2L,                "RTree Spatial Indexes",       "http://www.geopackage.org/guidance/extensions/rtree_spatial_indexes.html",
      3L,                 "Zoom Other Intervals",        "http://www.geopackage.org/guidance/extensions/zoom_other_intervals.html",
      4L,                  "Tiles Encoding WebP",         "http://www.geopackage.org/guidance/extensions/tiles_encoding_webp.html",
      5L,                             "Metadata",                    "http://www.geopackage.org/guidance/extensions/metadata.html",
      6L,                               "Schema",                      "http://www.geopackage.org/guidance/extensions/schema.html",
      7L, "WKT for Coordinate Reference Systems",                 "http://www.geopackage.org/guidance/extensions/wkt_for_crs.html",
      8L,          "Tiled Gridded Coverage Data", "http://www.geopackage.org/guidance/extensions/tiled_gridded_coverage_data.html",
      9L,             "Related Tables Extension",              "http://www.geopackage.org/guidance/extensions/related_tables.html"
    )

extensions_tables <-
    htmldf::html_df(gpkg_extensions$url)

gpkg_extensions$reference <-
    list(
    gpkg_geom_gname = list(
        gpkg_extensions = extensions_tables[[7]][[1]][[1]]
    ),
    gpkg_rtree_index = list(
        gpkg_extensions = extensions_tables[[7]][[2]][[1]]
    ),
    gpkg_zoom_other = list(
        gpkg_extensions = extensions_tables[[7]][[3]][[1]],
        gpkg_tile_matrix = NA
    ),
    gpkg_webp = list(
        gpkg_extensions = extensions_tables[[7]][[4]][[1]]
    ),
    gpkg_metadata = list(
        gpkg_extensions = extensions_tables[[7]][[5]][[1]],
        gpkg_metadata = extensions_tables[[7]][[5]][[2]],
        gpkg_metadata_reference = extensions_tables[[7]][[5]][[3]]
    ),
    gpkg_schema = list(
        gpkg_extensions = extensions_tables[[7]][[6]][[1]],
        gpkg_data_columns = extensions_tables[[7]][[6]][[2]],
        gpkg_data_column_constraints = list(
            enumeration = extensions_tables[[7]][[6]][[3]],
            range = extensions_tables[[7]][[6]][[4]],
            glob = extensions_tables[[7]][[6]][[5]],
            mime_type = NA
        )
    ),
    gpkg_crs_wkt = list(
        gpkg_extensions = extensions_tables[[7]][[7]][[1]],
        gpkg_spatial_ref_sys = as.data.frame(list(definition_12_063 = NA))
    ),
    gpkg_2d_gridded_coverage = list(
        gpkg_extensions = extensions_tables[[7]][[8]][[1]],
        gpkg_contents = as.data.frame(list(data_type = "2d-gridded-coverage")),
        gpkg_spatial_ref_sys = NA,
        gpkg_2d_gridded_coverage_ancillary = extensions_tables[[7]][[8]][[2]],
        # * Only used for “integer” datatype
        gpkg_2d_gridded_tile_ancillary = extensions_tables[[7]][[8]][[3]]
        # * Only used for “integer” datatype
        # ** These values are natural, not scaled or offset
    ),
    gpkg_related_tables = list(
        gpkg_extensions = extensions_tables[[7]][[9]][[1]],
        gpkgext_relations = extensions_tables[[7]][[9]][[2]]
        # NOTE: There are additional tables not captured in this index list
    )
)

usethis::use_data(gpkg_extensions, overwrite = TRUE)

community_gpkg_extensions <-
tibble::tribble(
    ~pos,                          ~extension,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ~description,                                                                                                       ~url,
      1L,            "Vector Tiles Extension",                                                                                                                                                                                                                                                                                                                                                                                                                           "Vector Tiles Extension defines how vector tiles can be stored in a GeoPackage.",                          "https://gitlab.com/imagemattersllc/ogc-vtp2/-/blob/master/extensions/1-vte.adoc",
      2L,     "Mapbox Vector Tiles Extension",                                                                                                                                                                                                                                                                                                                                                                                                  "Mapbox Vector Tiles Extension defines how vector tiles can be stored in the MapBox Vector Tiles format.",                         "https://gitlab.com/imagemattersllc/ogc-vtp2/-/blob/master/extensions/2-mvte.adoc",
      3L,    "GeoJSON Vector Tiles Extension",                                                                                                                                                                                                                                                                                                                                                                                                             "GeoJSON Vector Tiles Extension defines how vector tiles can be stored in the GeoJSON format.",                         "https://gitlab.com/imagemattersllc/ogc-vtp2/-/blob/master/extensions/3-gvte.adoc",
      4L, "Vector Tiles Attributes Extension",                                                                                                                                                                                                                                                                                                                                                  "Vector Tiles Attributes Extension defines how correlation between features and the tiles containing them using the GeoPackage Related Tables Extension.",                         "https://gitlab.com/imagemattersllc/ogc-vtp2/-/blob/master/extensions/4-vtae.adoc",
      5L,   "Styling and Symbology Extension",                                                                                                                                                                                                                                                                                                                                                                                                        "This extension defines how styles and symbols of arbitrary formats can be stored in a GeoPackage.",              "https://gitlab.com/imagemattersllc/ogc-tb-16-gpkg/-/blob/master/extensions/5-portrayal.adoc",
      6L,             "OWS Context Extension",                                                                                                                                                                                                                                                                                                                                                                                                                                   "This extension defines how OWS Contexts can be stored in a GeoPackage.", "https://gitlab.com/imagemattersllc/ogc-tb-16-gpkg/-/blob/master/extensions/31-owscontext_geopackage.adoc",
      7L,                 "Metadata Profiles",                                                                                                                                                                                                                                                                                                                                                                            "This extension allows for metadata profiles to be defined as extensions with a scope of \"metadata\". See below for examples.",      "https://gitlab.com/imagemattersllc/ogc-tb-16-gpkg/-/blob/master/extensions/7-metadata-profiles.adoc",
      8L,                "Generalized Tables",                                                                                                                                                                                                                                                                                                            "This extension defines the relationship between base tables and generalized tables. The objective here is to be able to define generalized tables in a way that maximizes client performance.",    "https://gitlab.com/imagemattersllc/ogc-tb-16-gpkg/-/blob/master/extensions/32-generalized-tables.adoc",
      9L,                      "Index Tables",                                                                                                                                                                                                                                                                                                                       "This extension defines describes how a GeoPackage can serve as an index for other GeoPackages. One use of this is to define a set of segment GeoPackages, for performance reasons.",          "https://gitlab.com/imagemattersllc/ogc-tb-16-gpkg/-/blob/master/extensions/33-index-tables.adoc",
     10L,                          "3D Tiles",                                                                                                                                                                                                                                                                                                                                                              "This extension represents 3D data in a GeoPackage using a relational data model in SQLite that mirrors the 3D Tiles spatial data structure.",                                       "http://www.compusult.net/html/OGC/3DTile_GeoPackage_Ext_Draft.html",
     11L,              "Semantic Annotations",                                                                                                                                                                                                                                                                                         "A semantic annotation is a semantically grounded term that can be applied to another concept. Use of this extension enables semantic annotations to be applied to any business object in the current GeoPackage.",  "https://gitlab.com/imagemattersllc/ogc-tb-16-gpkg/-/blob/master/extensions/13-semantic-annotations.adoc",
     12L,      "QGIS Map Styling Information",                                                                                                                                                                                                                                                                                                                                                                                            "This extension stores QGIS projects with their resources like images in print templates in a GeoPackage file.",                                    "https://github.com/pka/qgpkg/blob/master/qgis_geopackage_extension.md",
     13L,                 "Feature Tile Link",                                                                                                                                                                                                                                         "This extension creates a link between a feature and tile table. A tile table containing tiles that represent or were generated from features can be linked to the feature table. The link enables feature queries when dealing with tiles representing features.",                             "http://ngageoint.github.io/GeoPackage/docs/extensions/feature-tile-link.html",
     14L,                    "Geometry Index", "This extension defines a SQLite version agnostic way to index user feature table geometries by their bounding envelopes for fast ranged searches. Mobile implementations, including Android and iOS, use earlier versions of SQLite and can not rely on the R*Tree Module implementation. Each geometry in a feature table is indexed by its geometry id and x, y, z, and m value ranges. The geometry index can be queried for fast retrieval of only geometries overlapping a desired envelope bounds.",                                "http://ngageoint.github.io/GeoPackage/docs/extensions/geometry-index.html",
     15L,              "OWS Context (GeoCat)",                                                                                                                                                                                                "The main goal of the extension is to store context and styling of a mapping project as part of a GeoPackage file containing the data it refers to. The extension aims at similar use cases as presented in The USGS GeoPackage Styling Experiment in Testbed 12, however the approach is a bit different.",                    "https://github.com/GeoCat/geopackage-owc-spec/blob/master/owc_geopackage_extension.md"
    )

community_gpkg_extensions_tables <-
    htmldf::html_df(community_gpkg_extensions$url)

community_gpkg_extensions$reference <-
    list(
        im_vector_tiles = NA,
        im_vector_tiles_mapbox = NA,
        im_vector_tiles_geojson = NA,
        related_tables = NA,
        im_portrayal = NA,
        # NOTE: Could not find a name so I gave it ows_contexts as a placeholder
        ows_contexts = NA,
        im_metadata_profiles = NA,
        tb16_generalized = NA,
        tb16_index = NA,
        # NOTE: Could not find a name so I gave it tiles_3d as a placeholder
        tiles_3d = NA,
        im_semantic_annotations = NA,
        qgis = list(
            qgis_projects = community_gpkg_extensions_tables[[7]][[12]][[1]],
            qgis_resources = community_gpkg_extensions_tables[[7]][[12]][[2]],
            layer_styles = community_gpkg_extensions_tables[[7]][[12]][[3]]
        ),
        # NOTE: The next two returned a named list of data frames
        nga_feature_tile_link = community_gpkg_extensions_tables[[7]][[13]],
        nga_geometry_index = community_gpkg_extensions_tables[[7]][[14]],
        owc = list(
            owc_context = community_gpkg_extensions_tables[[7]][[15]][[1]],
            owc_resources = community_gpkg_extensions_tables[[7]][[15]][[2]],
            owc_style = community_gpkg_extensions_tables[[7]][[15]][[3]]
        )
    )

usethis::use_data(community_gpkg_extensions, overwrite = TRUE)

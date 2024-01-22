#' @title  Include leaflet to view images (BW or RGB)
#' @name bayes_view_image_raster
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  image         Image to be plotted
#' @param  map           Classified map to be overlayed on top on image
#' @param  red           Band for red color.
#' @param  green         Band for green color.
#' @param  blue          Band for blue color.
#' @param  legend        Named vector that associates labels to colors.
#' @param  palette       Palette provided in the configuration file.
#' @param  opacity       Opacity of overlayed map
#' @param  view_max_mb   Maximum size of leaflet to be visualized
#'
#' @return               A leaflet object.
#'
.view_image_raster <- function(image,
                               map,
                               red,
                               green,
                               blue,
                               legend,
                               palette,
                               opacity,
                               view_max_mb) {
    # set the view_max_mb parameter
    view_max_mb <- 64
    # find out if resampling is required (for big images)
    output_size <- .view_resample_size(
        image = image,
        ndates = length(dates),
        view_max_mb = 64
    )
    # create a leaflet and add providers
    leaf_map <- .view_add_basic_maps()
    # get names of basic maps
    base_maps <- .view_get_base_maps(leaf_map)
    # add RGB bands if required
    # create a leaflet for RGB bands
    leaf_map <- leaf_map |>
        .view_rgb_bands(
            cube = cube,
            red = red,
            green = green,
            blue = blue,
            dates = dates,
            output_size = output_size
        )
    # include class cube if available
    leaf_map <- leaf_map |>
        .view_map(
            map = map,
            tiles = tiles,
            legend = legend,
            palette = palette,
            opacity = opacity,
            output_size = output_size
        )
    # get overlay groups
    overlay_groups <- .view_add_overlay_grps(
        cube = cube,
        dates = dates,
        class_cube = class_cube
    )
    # add layers control to leafmap
    leaf_map <- leaf_map |>
        leaflet::addLayersControl(
            baseGroups = base_maps,
            overlayGroups = overlay_groups,
            options = leaflet::layersControlOptions(collapsed = FALSE)
        ) |>
        # add legend to leaf_map
        .view_add_legend(
            cube = class_cube,
            legend = legend,
            palette = palette
        )
    return(leaf_map)
}
#' @title  Return the size of the imaged to be resamples for visulization
#' @name .view_resample_size
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  image
#' @param  view_max_mb   Maximum size of leaflet to be visualized
#' @return               Number of rows and cols to be visualized.
#'
#'
.view_resample_size <- function(image, view_max_mb) {
    # number of tiles to be merged
    nrows <- nrow(image)
    ncols <- ncol(image)
    # get the compression factor
    leaflet_comp_factor <-  0.50
    # calculate the total size of all input images in bytes
    # note that leaflet considers 4 bytes per pixel
    in_size_mbytes <- 4 * nrows * ncols * comp
    # do we need to compress?
    ratio <- max((in_size_mbytes / (view_max_mb * 1024 * 1024)), 1)
    # only create local files if required
    if (ratio > 1) {
        new_nrows <- round(nrows / sqrt(ratio))
        new_ncols <- round(ncols * (new_nrows / nrows))
    } else {
        new_nrows <- round(nrows)
        new_ncols <- round(ncols)
    }
    leaflet_maxbytes <- 4 * new_nrows * new_ncols
    return(c(
        "xsize" = new_ncols, "ysize" = new_nrows,
        "leaflet_maxbytes" = leaflet_maxbytes
    ))
}
#' @title  Create a leafmap to view basic background maps
#' @name .view_add_basic_maps
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @return          Leafmap with maps from providers
#'
.view_add_basic_maps <- function() {
    # create a leaflet and add providers
    leaf_map <- leaflet::leaflet() |>
        leaflet::addProviderTiles(
            provider = leaflet::providers$GeoportailFrance.orthos,
            group = "GeoPortalFrance"
        ) |>
        leaflet::addProviderTiles(
            provider = leaflet::providers$Esri.WorldImagery,
            group = "ESRI"
        ) |>
        leaflet::addProviderTiles(
            provider = leaflet::providers$OpenStreetMap,
            group = "OSM"
        ) |>
        leaflet::addWMSTiles(
            baseUrl = "https://tiles.maps.eox.at/wms/",
            layers = c("s2cloudless-2020_3857_512"),
            group = "Sentinel-2-2020"
        ) |>
        leafem::addMouseCoordinates()
    return(leaf_map)
}
#' @title  Add base maps to leaflet map
#' @name .view_get_base_maps
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  leaf_map      Leaflet
#' @return               Base maps used in leaflet map
#'
.view_get_base_maps <- function(leaf_map) {
    base_maps <- purrr::map_chr(leaf_map$x$calls, function(c) {
        return(c$args[[3]])
    })
    return(base_maps)
}
#' @title  Include leaflet to view RGB bands
#' @name .view_rgb_bands
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  leafmap       Leaflet map
#' @param  image         SpatialRaster
#' @param  red           Band to be shown in red color
#' @param  green         Band to be shown in green color
#' @param  blue          Band to be shown in blue color
#' @param  output_size   Controls size of leaflet to be visualized
#' @return               A leaflet object
#
.view_rgb_bands <- function(leaf_map,
                            image,
                            red,
                            green,
                            blue,
                            dates,
                            output_size) {
    # obtain the raster objects for the dates chosen

    red_file   <- terra::sources(image[[red]])
    green_file <- terra::sources(image[[green]])
    blue_file  <- terra::sources(image[[blue]])

    rgb_files <- c(r = red_file, g = green_file, b = blue_file)
    st_obj <- stars::read_stars(
        rgb_files,
        along = "band",
        RasterIO = list(
            "nBufXSize" = output_size[["xsize"]],
            "nBufYSize" = output_size[["ysize"]]
        ),
                proxy = FALSE
    )
    # resample and warp the image
    st_obj_new <- stars::st_warp(
        src = st_obj,
        crs = sf::st_crs("EPSG:3857")
    )
    # add raster RGB to leaflet
    leaf_map <- leafem::addRasterRGB(
        leaf_map,
        x = st_obj_new,
        r = 1,
        g = 2,
        b = 3,
        quantiles = c(0.1, 0.9),
        project = FALSE,
        group = "RGB",
        maxBytes = output_size["leaflet_maxbytes"]
    )
    return(leaf_map)
}

#' @title  Include leaflet to view classified map
#' @name .view_map
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  leafmap       Leaflet map
#' @param  map           Classified map to be overlayed on top on image
#' @param  legend        Named vector that associates labels to colors.
#' @param  palette       Palette provided as alternative legend.
#' @param  opacity       Fill opacity
#' @param  output_size   Controls size of leaflet to be visualized
#'
.view_class_cube <- function(leaf_map,
                             map,
                             legend,
                             palette,
                             opacity,
                             output_size) {
    # should we overlay a classified image?
    # get the labels
    labels <- names(map)
    # obtain the colors
    colors <- .colors_get(
        labels = labels,
        legend = legend,
        palette = palette,
        rev = TRUE
    )
    # select the tiles that will be shown
    if (!purrr::is_null(tiles)) {
        class_cube <- dplyr::filter(
            class_cube,
            .data[["tile"]] %in% tiles
        )
    }

    # create the stars objects that correspond to the tiles
    st_objs <- slider::slide(class_cube, function(tile) {
        # obtain the raster stars object
        st_obj <- stars::read_stars(
            .tile_path(tile),
            RAT = labels,
            RasterIO = list(
                "nBufXSize" = output_size[["xsize"]],
                "nBufYSize" = output_size[["ysize"]]
            ),
            proxy = FALSE
        )
        return(st_obj)
    })
    # keep the first object
    st_merge <- st_objs[[1]]
    # if there is more than one stars object, merge them
    if (length(st_objs) > 1) {
        st_merge <- stars::st_mosaic(
            st_objs[[1]],
            st_objs[[2:length(st_objs)]]
        )
    }
    # resample and warp the image
    st_obj_new <- stars::st_warp(
        src = st_merge,
        crs = sf::st_crs("EPSG:3857")
    )
    # add the classified image object
    leaf_map <- leaf_map |>
        leafem::addStarsImage(
            x = st_obj_new,
            opacity = opacity,
            colors = colors,
            method = "ngb",
            group = "classification",
            project = FALSE,
            maxBytes = output_size["leaflet_maxbytes"]
        )
    return(leaf_map)
}
.view_add_stars_image <- function(leaf_map,
                                  band_file,
                                  tile,
                                  band,
                                  date,
                                  palette,
                                  output_size) {
    # create a stars object
    st_obj <- stars::read_stars(
        band_file,
        along = "band",
        RasterIO = list(
            "nBufXSize" = output_size[["xsize"]],
            "nBufYSize" = output_size[["ysize"]]
        ),
        proxy = FALSE
    )
    # warp the image
    st_obj_new <- stars::st_warp(
        src = st_obj,
        crs = sf::st_crs("EPSG:3857")
    )
    if (!purrr::is_null(date)) {
        group <- paste(tile[["tile"]], date)
    } else {
        group <- paste(tile[["tile"]], band)
    }
    # add stars to leaflet
    leaf_map <- leafem::addStarsImage(
        leaf_map,
        x = st_obj_new,
        band = 1,
        colors = palette,
        project = FALSE,
        group = group,
        maxBytes = output_size["leaflet_maxbytes"]
    )
    return(leaf_map)
}

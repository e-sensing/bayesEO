#' @title  Plot RGB data cubes
#' @name bayes_plot_rgb
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Plot RGB raster cube
#'
#' @param  image         Object of class SpatRaster.
#' @param  red           Band for red color.
#' @param  green         Band for green color.
#' @param  blue          Band for blue color.
#'
#' @return               A plot object with an RGB image
#' @examples
#' if (bayes_run_examples()) {
#' # Define location of a probability file
#' data_dir <- system.file("/extdata/rgb", package = "bayesEO")
#' # list the file
#' files <- list.files(data_dir)
#' # build the full path
#' image_files <- paste0(data_dir, "/", files)
#' rgb_image <- bayes_read_image(image_files)
#' bayes_plot_rgb(rgb_image, red = "B11", green = "B8A", blue = "B03")
#' }
#' @export
bayes_plot_rgb <- function(image,
                           red,
                           green,
                           blue) {

    # get RGB files for the requested timeline
    red_file   <- terra::sources(image[[red]])
    green_file <- terra::sources(image[[green]])
    blue_file  <- terra::sources(image[[blue]])
    rgb_files <- c(r = red_file, g = green_file, b = blue_file)

    # size of data to be read
    size <- .plot_read_size(image = image)

    # read raster data as a stars object with separate RGB bands
    rgb_st <- stars::read_stars(
        c(red_file, green_file, blue_file),
        along = "band",
        RasterIO = list(
            "nBufXSize" = size[["xsize"]],
            "nBufYSize" = size[["ysize"]]
        ),
        proxy = FALSE
    )
    rgb_st <- stars::st_rgb(rgb_st[, , , 1:3],
                            dimension = "band",
                            maxColorValue = 10000,
                            use_alpha = FALSE,
                            probs = c(0.05, 0.95),
                            stretch = TRUE
    )

    p <- tmap::tm_shape(rgb_st) +
        tmap::tm_raster() +
        tmap::tm_graticules(
            labels.size = 0.7
        ) +
        tmap::tm_compass()
    return(p)
}

#' @title  Plot probability and variance maps
#' @name   bayes_plot_probs
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param  x             SpatRaster to be plotted.
#' @param  scale         Scaling factor to apply to the data
#' @param  labels        Labels to be plotted
#' @param  palette       An RColorBrewer palette
#' @param  tmap_scale    Global scale parameter for map (default: 1.5)
#'
#' @return               A plot object
#'
#' @examples
#' if (bayes_run_examples()) {
#'     # get the probability file
#'     data_dir <- system.file("/extdata/probs/", package = "bayesEO")
#'     file <- list.files(data_dir)
#'     # build the full path
#'     probs_file <- paste0(data_dir, "/", file)
#'     # include the labels
#'     labels <- c("Water", "ClearCut_Burn", "ClearCut_Soil",
#'              "ClearCut_Veg", "Forest", "Wetland")
#'     # associate the labels to the names of the SpatRaster
#'     probs <- bayes_read_probs(probs_file, labels)
#'     # Plot the probability image
#'     bayes_plot_probs(probs,
#'                      scale = 0.0001,
#'                      tmap_scale = 1.5)
#' }
#'
#' @export
bayes_plot_probs <- function(x,
                       scale = 0.0001,
                       labels = NULL,
                       palette = "YlGnBu",
                       tmap_scale = 1.5){

    # check input image
    .check_that(
        "SpatRaster" %in% class(x),
        msg = "input is not SpatRaster type"
    )
    # get all labels to be plotted
    all_labels <- names(x)
    names(all_labels) <- seq_len(length(all_labels))
    # check the labels to be plotted
    # if NULL, use all labels
    if (purrr::is_null(labels))
        labels <- all_labels
    else
        .check_that(all(labels  %in% all_labels),
                    msg = "labels not in image")

    # read the file using stars
    probs_st <- stars::st_as_stars(x)
    # scale the data
    if (scale != 1) {
        probs_st <- probs_st * scale
    }
    # rename stars object dimensions to labels
    probs_st <- stars::st_set_dimensions(probs_st, "band",
                                         values = all_labels)
    # select stars bands to be plotted
    bds <- as.numeric(names(all_labels[all_labels %in% labels]))

    p <- suppressMessages(
        tmap::tm_shape(probs_st[, , , bds],
                       raster.downsample = FALSE) +
        tmap::tm_raster(style = "cont",
                        palette = palette,
                        midpoint = 0.5,
                        title = all_labels[all_labels %in% labels]) +
        tmap::tm_facets(free.coords = TRUE) +
        tmap::tm_compass() +
        # tmap::tm_layout(legend.show = TRUE,
        #                 legend.outside = FALSE,
        #                 legend.bg.color   = tmap_legend_bg_color,
        #                 legend.bg.alpha   = tmap_legend_bg_alpha,
        #                 legend.title.size = tmap_legend_title_size,
        #                 legend.text.size  =  tmap_legend_text_size,
        #                 outer.margins = 0)
        tmap::tm_layout(
            scale = tmap_scale,
            legend.show = TRUE,
            legend.outside = FALSE,
            legend.bg.color = "white",
            legend.bg.alpha = 0.5
        )
    )

    return(p)
}


#' @title  Plot labelled map
#' @name   bayes_plot_map
#' @author Gilberto Camara \email{gilberto.camara@@inpe.br}
#' @param  x                           SpatRaster to be plotted.
#' @param  legend                      Named vector that associates labels to colors.
#' @param  palette                     A sequential RColorBrewer palette
#' @param  tmap_graticules_labels_size Size of graticules labels (default: 0.7)
#' @param  tmap_legend_title_size      Size of legend title (default: 1.5)
#' @param  tmap_legend_text_size       Size of legend text (default: 1.2)
#' @param  tmap_legend_bg_color        Color of legend backgound (default: "white")
#' @param  tmap_legend_bg_alpha        Transparency of legend background (default: 0.5)
#' @param  tmap_max_cells              Maximum number of cells for tmap (default = 1e+06)
#'
#' @return               A plot object
#'
#' @examples
#' if (bayes_run_examples()) {
#'     # Define location of a probability file
#'     data_dir <- system.file("/extdata/Rondonia-20LLQ/probs", package = "bayesEO")
#'     # list the file
#'     file <- list.files(data_dir)
#'     # build the full path
#'     probs_file <- paste0(data_dir, "/", file)
#'     # define labels
#'     labels <- c("Water", "ClearCut_Burn", "ClearCut_Soil",
#'              "ClearCut_Veg", "Forest", "Wetland")
#'
#'     probs_image <- bayes_read_probs(probs_file, labels)
#'     # Label the probs image
#'     y <- bayes_label(x)
#'     # produce a map of the labelled image
#'     bayes_plot_map(y)
#' }
#'
#' @export
bayes_plot_map <- function(x,
                      legend = NULL,
                      palette = "Spectral",
                      tmap_graticules_labels_size = 0.7,
                      tmap_legend_title_size = 0.7,
                      tmap_legend_text_size = 0.7,
                      tmap_legend_bg_color = "white",
                      tmap_legend_bg_alpha = 0.5,
                      tmap_max_cells = 1e+06) {
    # check input image
    .check_that(
        "SpatRaster" %in% class(x),
        msg = "input is not SpatRaster type"
    )
    # check that input is a map
    .check_that(
        terra::nlyr(x) == 1,
        msg = "input is not a categorical map"
    )
    labels <- terra::levels(x)[[1]]$class
    # names(labels) <- seq_along(labels)
    # obtain the colors
    colors <- .color_get_labels(
        labels = labels,
        legend = legend,
        palette = palette
    )
    # read the file using stars
    stars_obj <- stars::st_as_stars(x)

    # plot using tmap
    # tmap requires numbers, not names    # rename stars object
    stars_obj <- stats::setNames(stars_obj, "labels")
    p <- suppressMessages(tmap::tm_shape(stars_obj,
                       raster.downsample = FALSE) +
            tmap::tm_raster(
                style = "cat",
                palette = colors,
                labels = labels) +
            tmap::tm_graticules(
                labels.size = tmap_graticules_labels_size
            )  +
            tmap::tm_compass() +
            tmap::tm_layout(
                legend.show = TRUE,
                legend.outside = FALSE,
                legend.title.size = tmap_legend_title_size,
                legend.text.size = tmap_legend_text_size,
                legend.bg.color = tmap_legend_bg_color,
                legend.bg.alpha = tmap_legend_bg_alpha)
    )
    return(p)

}
#' @title  Plot histogram
#' @name   bayes_plot_hist
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param  x             SpatRaster to be plotted.
#' @param  scale         Scale factor for SpatRaster
#' @param  quantile      Threshold of values that will be plotted
#' @param  sample_size   Number of samples to extract values
#'
#' @return               A plot object
#' @examples
#' if (bayes_run_examples()) {
#'     # get the probability file
#'     data_dir <- system.file("/extdata/probs/", package = "bayesEO")
#'     file <- list.files(data_dir)
#'     # read the probability file into a SpatRaster
#'     x <- terra::rast(paste0(data_dir, "/", file))
#'     # include the labels
#'     labels <- c("Water", "ClearCut_Burn", "ClearCut_Soil",
#'              "ClearCut_Veg", "Forest", "Wetland")
#'     # associate the labels to the names of the SpatRaster
#'     names(x) <- labels
#'     # calculate the variance
#'     v <- bayes_variance(x)
#'     # Plot the variance histogram
#'     bayes_hist(v, quantile = 0.75)
#'}
#'
#' @export
bayes_plot_hist <- function(x,
                       scale = 1,
                       quantile = NULL,
                       sample_size = 15000) {

    # take a sample from points inside the SpatVector
    vec <- terra::vect(terra::ext(x), crs = terra::crs(x))
    points <- terra::spatSample(vec, size = sample_size)
    # extract values
    values <- terra::extract(x, points, na.rm = TRUE)
    # remove first column
    values <- values[,-1]
    # scale the values
    if (scale != 1)
        values <- values * scale
    # select the values above the quantile
    if (!purrr::is_null(quantile)) {
        values <- lapply(
            colnames(values), function(x) {
                vls <- values[,x]
                quant <- stats::quantile(vls, quantile)
                vls[vls < quant] <- NA
                return(vls)
            })
        values <- do.call(cbind, values)
        colnames(values) <- names(x)
    }
    # convert to tibble
    values <- tibble::as_tibble(values)
    # include label names
    colnames(values) <- names(x)
    # dissolve the data for plotting
    values <- tidyr::pivot_longer(values,
                                  cols = tidyr::everything(),
                                  names_to = "labels",
                                  values_to = "variance")
    # Histogram with density plot
    p <- suppressWarnings(
        ggplot2::ggplot(values,
                         ggplot2::aes(x = .data[["variance"]])) +
        ggplot2::geom_histogram(binwidth = 1,
                                fill  = "#69b3a2",
                                color = "#e9ecef",
                                alpha = 0.9) +
        ggplot2::scale_x_continuous()
    )
    p <- p + ggplot2::facet_wrap(facets = "labels")

    return(p)
}

#' @title  Return the cell size for the image to be reduced for plotting
#' @name .plot_read_size
#' @keywords internal
#' @noRd
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @param  image         Image to be plotted.
#' @return               Cell size for x and y coordinates.
#'
#'
.plot_read_size <- function(image) {
    # get the maximum number of bytes to be displayed
    max_cells <- 1e+07
    # numbers of nrows and ncols
    nrows <- nrow(image)
    ncols <- ncol(image)

    # do we need to compress?
    ratio <- max((nrows * ncols / max_cells), 1)
    # only create local files if required
    if (ratio > 1) {
        new_nrows <- round(nrows / sqrt(ratio))
        new_ncols <- round(ncols * (new_nrows / nrows))
    } else {
        new_nrows <- round(nrows)
        new_ncols <- round(ncols)
    }
    return(c(
        "xsize" = new_ncols, "ysize" = new_nrows
    ))
}

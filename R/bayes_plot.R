#' @title  Plot probability and variance maps
#' @name   bayes_plot
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param  x             SpatRaster to be plotted.
#' @param  scale         Scaling factor to apply to the data
#' @param  labels        Labels to be plotted
#' @param  quantile      Thereshold of values to be plotted
#' @param  palette       An RColorBrewer palette
#' @param  tmap_legend_title_size  Size of legend title (default: 1.5)
#' @param  tmap_legend_text_size   Size of legend text (default: 1.2)
#' @param  tmap_legend_bg_color    Color of legend backgound (default: "white")
#' @param  tmap_legend_bg_alpha    Transparency of legend background (default: 0.5)
#'
#' @return               A plot object
#'
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
#'     # Plot the probability image
#'     bayes_plot(x, scale = 0.0001, labels = c("Forest", "ClearCut_Soil"))
#' }
#'
#' @export
bayes_plot <- function(x,
                       scale = 1,
                       labels = NULL,
                       quantile = NULL,
                       palette = "YlGnBu",
                       tmap_legend_title_size = 1.0,
                       tmap_legend_text_size = 1.0,
                       tmap_legend_bg_color = "white",
                       tmap_legend_bg_alpha = 0.5) {

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

    if (!purrr::is_null(quantile)) {
        # get values
        values <- terra::values(x)
        # show only the chosen quantile
        values <- lapply(
            colnames(values), function(name) {
                vls <- values[,name]
                quant <- stats::quantile(vls, quantile, na.rm = TRUE)
                vls[vls < quant] <- NA
                return(vls)
            })
        values <- do.call(cbind, values)
        colnames(values) <- names(x)
        terra::values(x) <- values
    }
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

    p <- tmap::tm_shape(probs_st[, , , bds]) +
        tmap::tm_raster(style = "cont",
                        palette = palette,
                        midpoint = 0.5,
                        title = all_labels[all_labels %in% labels]) +
        tmap::tm_facets(free.coords = TRUE) +
        tmap::tm_compass() +
        tmap::tm_layout(legend.show = TRUE,
                        legend.outside = FALSE,
                        legend.bg.color   = tmap_legend_bg_color,
                        legend.bg.alpha   = tmap_legend_bg_alpha,
                        legend.title.size = tmap_legend_title_size,
                        legend.text.size  =  tmap_legend_text_size,
                        outer.margins = 0)

    return(p)
}


#' @title  Plot labelled map
#' @name   bayes_map
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
#'     # Label the probs image
#'     y <- bayes_label(x)
#'     # produce a map of the labelled image
#'     bayes_map(y)
#' }
#'
#' @export
bayes_map <- function(x,
                      legend = NULL,
                      palette = "Spectral",
                      tmap_graticules_labels_size = 0.7,
                      tmap_legend_title_size = 1.0,
                      tmap_legend_text_size = 1.0,
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
    names(colors) <- seq_along(names(colors))
    p <- suppressWarnings(
        tmap::tm_shape(stars_obj) +
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
#' @name   bayes_hist
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
bayes_hist <- function(x,
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

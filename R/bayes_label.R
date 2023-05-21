#' @title Label probability images to create categorical maps
#'
#' @name  bayes_label
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a classified image with probabilities, and
#' labels the image with the pixel of higher probability
#'
#' @param  x                 SpatRaster object with probabilities images
#'
#' @return A SpatRaster object
#'
#' @examples
#' if (bayes_run_examples()) {
#'     # select a file with probability values
#'     data_dir <- system.file("/extdata/probs/", package = "bayesEO")
#'     file <- list.files(data_dir)
#'     # create a SpatRaster object from the file
#'     x <- terra::rast(paste0(data_dir, "/", file))
#'     # provide the labels
#'     labels <- c("Water", "ClearCut_Burn", "ClearCut_Soil",
#'              "ClearCut_Veg", "Forest", "Wetland")
#'     # name the layers in the SpatRaster with the labels
#'     names(x) <- labels
#'     # produce a labelled map
#'     y <- bayes_label(x)
#'     # plot the labelled map
#'     bayes_map(y)
#'}
#'
#' @export
bayes_label <- function(x){
    # check input image
    .check_that(
        "SpatRaster" %in% class(x),
        msg = "input is not SpatRaster type"
    )
    # read values
    values <- terra::values(x)
    # get maximum prob
    values <- C_label_max_prob(values)
    # create SpatRaster
    y <- terra::rast(x, nlyrs = 1)
    terra::values(y) <- values
    # label the classified image
    nlabels <- length(names(x))
    levels(y) <- data.frame(id = 1:nlabels, class = names(x))
    return(y)
}

#' @title Summary of categorical maps
#'
#' @name  bayes_summary
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a classified image with probabilities, and
#' labels the image with the pixel of higher probability
#'
#' @param  x                 SpatRaster categorical object
#' @param  scale             Scale to apply to data
#' @param  sample_size       Sample size
#'
#' @return A tibble with information
#'
#' @examples
#' if (bayes_run_examples()) {
#'     # select a file with probability values
#'     data_dir <- system.file("/extdata/probs/", package = "bayesEO")
#'     file <- list.files(data_dir)
#'     # create a SpatRaster object from the file
#'     x <- terra::rast(paste0(data_dir, "/", file))
#'     # provide the labels
#'     labels <- c("Water", "ClearCut_Burn", "ClearCut_Soil",
#'              "ClearCut_Veg", "Forest", "Wetland")
#'     # name the layers in the SpatRaster with the labels
#'     names(x) <- labels
#'     # produce a labelled map
#'     y <- bayes_label(x)
#'     # plot the labelled map
#'     bayes_summary(y)
#'}
#'
#' @export
bayes_summary <- function(x, scale = 1, sample_size = 15000){
    # check input image
    .check_that(
        "SpatRaster" %in% class(x),
        msg = "input is not SpatRaster type"
    )
    if (all(terra::is.factor(x))) {
        freq <- terra::freq(x)
        area_pixel <- terra::xres(x)*terra::yres(x)
        area_km2 <- freq$count*area_pixel/1e+06
        area_km2 <- round(area_km2, 2)
        sum <- tibble::tibble(
            class = terra::levels(x)[[1]]$class,
            area  = area_km2
        )
    }
    else {
        # get the a sample of the values
        values <- x |>
            terra::spatSample(size = sample_size, na.rm = TRUE)
        # scale the values
        sum <- summary(values*scale)
        colnames(sum) <- names(x)
    }
    return(sum)
}

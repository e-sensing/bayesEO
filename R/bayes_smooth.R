#' @title Smooth probability images
#'
#' @name  bayes_smooth
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Takes a classified image with probabilities, and reduces outliers
#' and smoothens probability according to Bayesian statistics
#'
#' @param  x                 SpatRaster object with probabilities images
#' @param  window_size       Size of the neighborhood.
#' @param  neigh_fraction    Fraction of neighbors with high probabilities
#'                           to be used in Bayesian inference.
#' @param  smoothness        Estimated variance of logit of class probabilities
#'                           (Bayesian smoothing parameter). It can be either
#'                           a vector or a scalar.
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
#'     # smooth the probability image
#'     y <- bayes_smooth(x,
#'             window_size = 7,
#'             neigh_fraction = 0.5,
#'             smoothness = 20
#'     )
#'     # plot the probability image
#'     bayes_plot(y, scale = 0.0001)
#'}
#'
#' @export
bayes_smooth <- function(x,
                         window_size = 7,
                         neigh_fraction = 0.5,
                         smoothness = 10){
    # check input image
    .check_that(
        "SpatRaster" %in% class(x),
        msg = "input is not SpatRaster type"
    )
    labels <- names(x)
    # Check window size
    .check_window_size(window_size, min = 5)
    # Check neigh_fraction
    .check_num_parameter(neigh_fraction, exclusive_min = 0, max = 1)
    # Check smoothness
    .check_smoothness(smoothness, length(labels))
    # Prepare smoothness parameter
    if (length(smoothness == 1)) {
        smoothness <- rep(smoothness, length(labels))
    }
    # read values
    values <- terra::values(x)
    # deduce scale
    scale <- 1/max(values)
    # scale values
    values <- values * scale
    # avoid zero
    values[values < 0.0001] <- 0.0001
    # avoid one
    values[values > 0.9999] <- 0.9999
    # Compute logit
    values <- log(values / (rowSums(values) - values))
    # Process Bayesian
    values <- bayes_smoother_fraction(
        logits = values,
        nrows  = terra::nrow(x),
        ncols  = terra::ncol(x),
        window_size = window_size,
        smoothness = smoothness,
        neigh_fraction = neigh_fraction
    )
    # Compute inverse logit
    values <- exp(values) / (exp(values) + 1)
    # rescale the data
    values <- values / scale
    # create SpatRaster
    y <- terra::rast(x)
    terra::values(y) <- values
    names(y) <- names(x)
    return(y)
}

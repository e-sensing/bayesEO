#' @title Calculate the variance of a probability cube
#'
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#'
#' @description Takes a probability cube and estimate the local variance
#'              of the logit of the probability,
#'              to support the choice of parameters for Bayesian smoothing.
#'
#' @param  x                 SpatRaster object containing probabilities.
#' @param  window_size       Size of the neighborhood.
#' @param  neigh_fraction    Fraction of neighbors with highest probability
#'                           to be used in Bayesian inference.
#' @return A variance SpatRaster object.
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
#'     # calculate the variance
#'     v <- bayes_variance(x)
#'     # plot the variance
#'     bayes_plot_var(v, quantile = 0.75)
#' }
#' @export
bayes_variance <- function(x,
                           window_size = 9,
                           neigh_fraction = 0.5) {

    # check input image
    .check_that(
        "SpatRaster" %in% class(x),
        msg = "input is not SpatRaster type"
    )
    # Check window size
    .check_window_size(window_size, min = 5)
    # Check neigh_fraction
    .check_num_parameter(neigh_fraction, exclusive_min = 0, max = 1)

    # Create a window
    window <- matrix(1, nrow = window_size, ncol = window_size)
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
    # Define transform to logits
    values <- log(values / (rowSums(values) - values))
    # Process variance
    values <- bayes_var(
        m = values,
        m_nrow = terra::nrow(x),
        m_ncol = terra::ncol(x),
        w = window,
        neigh_fraction = neigh_fraction)
    # Return SpatRaster
    y <- terra::rast(x)
    terra::values(y) <- values
    names(y) <- names(x)
    return(y)
}
